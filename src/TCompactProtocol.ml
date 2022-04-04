(*
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements. See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership. The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied. See the License for the
 specific language governing permissions and limitations
 under the License.
*)

open Thrift
open EndianBytes

module P = Protocol

(* constants *)
let protocol_id = 0x82
let version = 1
let version_mask = 0x1f
let type_bits = 0x07
let type_shift_amount = 5

module CompactType = struct
  exception Parse_error of string

  type t =
    | STOP
    | TRUE
    | FALSE
    | BYTE
    | I16
    | I32
    | I64
    | DOUBLE
    | BINARY
    | LIST
    | SET
    | MAP
    | STRUCT
  [@@deriving show]

  let of_int = function
    | 0x00 -> STOP
    | 0x01 -> TRUE
    | 0x02 -> FALSE
    | 0x03 -> BYTE
    | 0x04 -> I16
    | 0x05 -> I32
    | 0x06 -> I64
    | 0x07 -> DOUBLE
    | 0x08 -> BINARY
    | 0x09 -> LIST
    | 0x0A -> SET
    | 0x0B -> MAP
    | 0x0C -> STRUCT
    | n -> raise (Parse_error (Printf.sprintf "Unknown code: 0x%X" n))

  let to_int = function
    | STOP -> 0x00
    | TRUE -> 0x01
    | FALSE -> 0x02
    | BYTE -> 0x03
    | I16 -> 0x04
    | I32 -> 0x05
    | I64 -> 0x06
    | DOUBLE -> 0x07
    | BINARY -> 0x08
    | LIST -> 0x09
    | SET -> 0x0A
    | MAP -> 0x0B
    | STRUCT -> 0x0C

  let to_t_type = function
    | STOP -> P.T_STOP
    | TRUE | FALSE -> P.T_BOOL
    | BYTE -> P.T_BYTE
    | I16 -> P.T_I16
    | I32 -> P.T_I32
    | I64 -> P.T_I64
    | DOUBLE -> P.T_DOUBLE
    | BINARY -> P.T_STRING
    | STRUCT -> P.T_STRUCT
    | LIST -> P.T_LIST
    | SET -> P.T_SET
    | MAP -> P.T_MAP

  let of_t_type = function
    | P.T_STOP -> STOP
    | P.T_BOOL -> TRUE
    | P.T_BYTE -> BYTE
    | P.T_I16 -> I16
    | P.T_I32 -> I32
    | P.T_I64 -> I64
    | P.T_DOUBLE -> DOUBLE
    | P.T_STRING -> BINARY
    | P.T_STRUCT -> STRUCT
    | P.T_LIST -> LIST
    | P.T_SET -> SET
    | P.T_MAP -> MAP
    | P.T_BINARY -> BINARY
    | P.T_VOID | P.T_I08 | P.T_U64
    | P.T_UTF7 | P.T_UTF8 | P.T_UTF16 as x ->
      raise (P.E (P.INVALID_DATA, Printf.sprintf "TType '%s' cannot be converted to CType" @@ P.show_t_type x))
end

module type IntSig = sig
  type t
  val logor : t -> t -> t
  val logand : t -> t -> t
  val logxor : t -> t -> t
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val neg : t -> t
  val one : t
  val zero : t
  val of_int : int -> t
  val to_int : t -> int
  val to_string : t -> string
  val equal : t -> t -> bool
  val max_int : t
  val min_int : t
end

module IntOps (I : IntSig) = struct
  let (lor) = I.logor
  let (land) = I.logand
  let (lxor) = I.logxor
  let (lsl) = I.shift_left
  let (lsr) = I.shift_right_logical
  let (asr) = I.shift_right
  let (~-) = I.neg
end

module ReadWriteInt (I : IntSig) = struct
  let write_varint trans (n : I.t) =
    let buffer = Buffer.create 8 in
    let open I in
    let open IntOps(I) in
    let rec write_varint_aux (i : I.t) =
      let b = i land (of_int 0x7F) in
      let i = i lsr 7 in
      Buffer.add_uint8 buffer
        @@ to_int
        @@ if i <> zero then b lor of_int 128 else b;
      if i = zero then Buffer.contents buffer else write_varint_aux i
    in
    let st = write_varint_aux n in
    trans#write_string st 0 @@ String.length st

  let read_varint trans buffer =
    let open I in
    let open IntOps(I) in
    let rec read_varint_aux (current_result : I.t) (shift_by : int) : I.t =
      trans#readAll buffer 0 1 |> ignore;
      let byte = LittleEndian.get_int8 buffer 0 in
      let current_result = current_result lor (of_int (Int.logand byte 0x7f) lsl shift_by) in
      if Int.shift_right_logical byte 7 = 0 then
        current_result
      else
        read_varint_aux current_result (shift_by + 7)
    in

    read_varint_aux zero 0

  let from_zig_zag (n : I.t) : I.t =
    let open IntOps(I) in
    (n lsr 1) lxor ~-(n land I.one)

  let to_zig_zag (n : I.t) (bits : int) : I.t =
    let open IntOps(I) in
    (n lsl 1) lxor (n asr (bits - 1))
end

module RI   = ReadWriteInt(
  struct
    include Int
    let of_int i = i
    let to_int i = i
  end)
module RI32 = ReadWriteInt(Int32)
module RI64 = ReadWriteInt(Int64)

class t trans =
object (self)
  inherit P.t trans

  (* private *)
  val mutable _last_fid = 0
  val mutable _bool_fid = None
  val mutable _bool_value = None
  val mutable _structs = []
  val _ibyte = Bytes.create 8

  method writeBool b =
    let comp_type = CompactType.(if b then TRUE else FALSE) in
    match _bool_fid with
    | Some x ->
        self#writeFieldHeader (CompactType.to_int comp_type) x;
        _bool_fid <- None
    | None ->
        self#writeByte @@ CompactType.to_int comp_type

  method writeByte i =
    BigEndian.set_int8 _ibyte 0 i;
    trans#write _ibyte 0 1

  method writeI16 i = RI.(write_varint trans @@ to_zig_zag i Sys.int_size)

  method writeI32 i = RI32.(write_varint trans @@ to_zig_zag i 32)

  method writeI64 i = RI64.(write_varint trans @@ to_zig_zag i 64)

  method writeDouble d =
    LittleEndian.set_double _ibyte 0 d;
    trans#write _ibyte 0 8

  method writeString s =
    RI.write_varint trans @@ String.length s;
    trans#write_string s 0 @@ String.length s

  method writeBinary a = self#writeString a

  method writeMessageBegin (n,t,s) =
    self#writeUByte protocol_id;
    let message_type = P.message_type_to_i t in
    self#writeUByte @@ version lor (message_type lsl type_shift_amount);
    RI.write_varint trans s;
    self#writeString n

  method writeMessageEnd = ()

  method writeStructBegin _s =
    _structs <- _last_fid :: _structs;
    _last_fid <- 0

  method writeStructEnd =
    _last_fid <- List.hd _structs;
    _structs <- List.tl _structs

  method writeFieldBegin (_n,t,i) =
    if t = P.T_BOOL then
      _bool_fid <- Some i
    else begin
      let ctype = CompactType.(to_int @@ of_t_type t) in
      self#writeFieldHeader ctype i
    end

  method writeFieldEnd = ()
  method writeFieldStop = self#writeByte 0
  method writeMapBegin (k,v,s) =
    if s = 0 then
      self#writeByte 0
    else begin
      RI.write_varint trans s;
      let open CompactType in
      let k = to_int @@ of_t_type k in
      let v = to_int @@ of_t_type v in
      self#writeUByte @@ (k lsl 4) lor v
    end

  method writeMapEnd = ()

  method writeListBegin (t,s) =
    self#writeCollectionBegin t s

  method writeListEnd = ()

  method writeSetBegin (t,s) =
    self#writeCollectionBegin t s

  method writeSetEnd = ()

  method private writeCollectionBegin etype size =
    let ctype = CompactType.(to_int @@ of_t_type etype) in
    if size <= 14 then
      self#writeUByte @@ (size lsl 4) lor ctype
    else begin
      self#writeUByte @@ 0xF0 lor ctype;
      RI.write_varint trans size
    end

  method private writeFieldHeader field_type fid =
    let delta = fid - _last_fid in
    if 0 < delta && delta <= 15 then
      self#writeUByte ((delta lsl 4) lor field_type)
    else begin
      self#writeByte field_type;
      self#writeI16 fid
    end;
    _last_fid <- fid

  method private writeUByte v =
    BigEndian.set_int8 _ibyte 0 v;
    trans#write _ibyte 0 1

  method readByte =
    trans#readAll _ibyte 0 1 |> ignore;
    BigEndian.get_int8 _ibyte 0

  method readI16 = RI.from_zig_zag @@ RI.read_varint trans _ibyte
  method readI32 = RI32.from_zig_zag @@ RI32.read_varint trans _ibyte
  method readI64 = RI64.from_zig_zag @@ RI64.read_varint trans _ibyte

  method readDouble =
    trans#readAll _ibyte 0 8 |> ignore;
    LittleEndian.get_double _ibyte 0

  method readBool =
    match _bool_value with
    | Some x ->
      _bool_value <- None;
      x
    | None -> self#readByte = CompactType.(to_int TRUE)

  method readString =
    let length = self#readSize in
    let buffer = Bytes.create length in
    trans#readAll buffer 0 length |> ignore;
    Bytes.unsafe_to_string buffer

  method readBinary = self#readString

  method readMessageBegin =
    let proto_id = self#readUByte in
    if proto_id <> protocol_id then
      raise (P.E (BAD_VERSION, Printf.sprintf "Bad protocol id in the message %x" proto_id))
    else begin
    let ver_type = self#readUByte in
    let message_type = P.message_type_of_i ((ver_type lsr type_shift_amount) land type_bits) in
    let message_version = ver_type land version_mask in
    if message_version <> version then
      raise (P.E (P.BAD_VERSION, Printf.sprintf "Bad version: %x (expect %x)" message_version version))
    else begin
    let sequence_id = self#readVarint in
    let message_name = self#readString in
    message_name, message_type, sequence_id
    end end

  method readMessageEnd =
    assert (List.length _structs = 0)

  method readStructBegin =
    _structs <- _last_fid :: _structs;
    _last_fid <- 0;
    ""

  method readStructEnd =
    _last_fid <- List.hd _structs;
    _structs <- List.tl _structs

  method readFieldBegin =
    let field_type = self#readUByte in
    if field_type land 0x0f = P.t_type_to_i P.T_STOP then
      "", T_STOP, 0
    else begin
    let delta = field_type lsr 4 in
    let fid = if delta = 0 then
        RI.from_zig_zag self#readVarint
      else
        _last_fid + delta
    in

    _last_fid <- fid;

    let field_type = field_type land 0x0f in

    if field_type = CompactType.(to_int TRUE) then
      _bool_value <- Some true
    else if field_type = CompactType.(to_int FALSE) then
      _bool_value <- Some false
    else
      ()
    ;

    let field_type = P.t_type_to_i @@ CompactType.(to_t_type @@ of_int field_type) in
    "", P.t_type_of_i field_type, fid
    end

  method readFieldEnd = ()

  method readMapBegin =
    let size = self#readSize in
    let types = if size > 0 then self#readUByte else 0 in
    let value_type = CompactType.(to_t_type @@ of_int @@ types land 0x0F) in
    let key_type = CompactType.(to_t_type @@ of_int @@ (types lsr 4) land 0x0F) in
    key_type, value_type, size

  method readMapEnd = ()

  method readListBegin = self#readCollectionBegin
  method readListEnd = ()
  method readSetBegin = self#readCollectionBegin
  method readSetEnd = ()

  method private readSize =
    let res = self#readVarint in
    if res < 0 then raise (P.E (P.INVALID_DATA, "Length < 0"))
    else res

  method private readUByte =
    trans#readAll _ibyte 0 1 |> ignore;
    BigEndian.get_uint8 _ibyte 0

  method private readCollectionBegin =
    let size_and_type = self#readUByte in
    let col_type = CompactType.(to_t_type @@ of_int @@ size_and_type land 0x0F) in
    let col_size = size_and_type lsr 4 in
    let col_size = if col_size = 15 then self#readSize else col_size in
    col_type, col_size

  method private readVarint =
    RI.read_varint trans _ibyte
end

class factory =
object
  inherit P.factory
  method getProtocol tr = new t tr
end
