(*
 Autogenerated by Thrift Compiler (0.16.0)

 DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING
*)

open Thrift
class nestedStruct =
object (self)
  val mutable _message : string option = None
  method get_message = _message
  method grab_message = match _message with None->raise (Field_empty "nestedStruct.message") | Some _x0 -> _x0
  method set_message _x0 = _message <- Some _x0
  method unset_message = _message <- None
  method reset_message = _message <- None

  method copy =
      let _new = Oo.copy self in
    _new
  method write (oprot : Protocol.t) =
    oprot#writeStructBegin "NestedStruct";
    (match _message with None -> () | Some _v -> 
      oprot#writeFieldBegin("message",Protocol.T_STRING,1);
      oprot#writeString(_v);
      oprot#writeFieldEnd
    );
    oprot#writeFieldStop;
    oprot#writeStructEnd
end
let rec read_nestedStruct (iprot : Protocol.t) =
  let _str3 = new nestedStruct in
    ignore(iprot#readStructBegin);
    (try while true do
        let (_,_t4,_id5) = iprot#readFieldBegin in
        if _t4 = Protocol.T_STOP then
          raise Break
        else ();
        (match _id5 with 
          | 1 -> (if _t4 = Protocol.T_STRING then
              _str3#set_message iprot#readString
            else
              iprot#skip _t4)
          | _ -> iprot#skip _t4);
        iprot#readFieldEnd;
      done; ()
    with Break -> ());
    iprot#readStructEnd;
    _str3

class testStruct =
object (self)
  val mutable _int32 : Int32.t option = None
  method get_int32 = _int32
  method grab_int32 = match _int32 with None->raise (Field_empty "testStruct.int32") | Some _x7 -> _x7
  method set_int32 _x7 = _int32 <- Some _x7
  method unset_int32 = _int32 <- None
  method reset_int32 = _int32 <- None

  val mutable _long : Int64.t option = None
  method get_long = _long
  method grab_long = match _long with None->raise (Field_empty "testStruct.long") | Some _x8 -> _x8
  method set_long _x8 = _long <- Some _x8
  method unset_long = _long <- None
  method reset_long = _long <- None

  val mutable _trueVal : bool = true
  method get_trueVal = Some _trueVal
  method grab_trueVal = _trueVal
  method set_trueVal _x9 = _trueVal <- _x9
  method reset_trueVal = _trueVal <- true

  val mutable _falseVal : bool option = None
  method get_falseVal = _falseVal
  method grab_falseVal = match _falseVal with None->raise (Field_empty "testStruct.falseVal") | Some _x10 -> _x10
  method set_falseVal _x10 = _falseVal <- Some _x10
  method unset_falseVal = _falseVal <- None
  method reset_falseVal = _falseVal <- None

  val mutable _decimal : float option = None
  method get_decimal = _decimal
  method grab_decimal = match _decimal with None->raise (Field_empty "testStruct.decimal") | Some _x11 -> _x11
  method set_decimal _x11 = _decimal <- Some _x11
  method unset_decimal = _decimal <- None
  method reset_decimal = _decimal <- None

  val mutable _comment : string option = None
  method get_comment = _comment
  method grab_comment = match _comment with None->raise (Field_empty "testStruct.comment") | Some _x12 -> _x12
  method set_comment _x12 = _comment <- Some _x12
  method unset_comment = _comment <- None
  method reset_comment = _comment <- None

  val mutable _nested : nestedStruct option = None
  method get_nested = _nested
  method grab_nested = match _nested with None->raise (Field_empty "testStruct.nested") | Some _x13 -> _x13
  method set_nested _x13 = _nested <- Some _x13
  method unset_nested = _nested <- None
  method reset_nested = _nested <- None

  val mutable _bools : bool list option = None
  method get_bools = _bools
  method grab_bools = match _bools with None->raise (Field_empty "testStruct.bools") | Some _x14 -> _x14
  method set_bools _x14 = _bools <- Some _x14
  method unset_bools = _bools <- None
  method reset_bools = _bools <- None

  method copy =
      let _new = Oo.copy self in
      if _nested <> None then
        _new#set_nested self#grab_nested#copy;
    _new
  method write (oprot : Protocol.t) =
    oprot#writeStructBegin "TestStruct";
    (match _int32 with None -> () | Some _v -> 
      oprot#writeFieldBegin("int32",Protocol.T_I32,1);
      oprot#writeI32(_v);
      oprot#writeFieldEnd
    );
    (match _long with None -> () | Some _v -> 
      oprot#writeFieldBegin("long",Protocol.T_I64,2);
      oprot#writeI64(_v);
      oprot#writeFieldEnd
    );
    (match _trueVal with true -> () | _v -> 
      oprot#writeFieldBegin("trueVal",Protocol.T_BOOL,3);
      oprot#writeBool(_v);
      oprot#writeFieldEnd
    );
    (match _falseVal with None -> () | Some _v -> 
      oprot#writeFieldBegin("falseVal",Protocol.T_BOOL,4);
      oprot#writeBool(_v);
      oprot#writeFieldEnd
    );
    (match _decimal with None -> () | Some _v -> 
      oprot#writeFieldBegin("decimal",Protocol.T_DOUBLE,5);
      oprot#writeDouble(_v);
      oprot#writeFieldEnd
    );
    (match _comment with 
    | None -> raise (Field_empty "testStruct._comment")
    | Some _v -> 
      oprot#writeFieldBegin("comment",Protocol.T_STRING,6);
      oprot#writeString(_v);
      oprot#writeFieldEnd
    );
    (match _nested with None -> () | Some _v -> 
      oprot#writeFieldBegin("nested",Protocol.T_STRUCT,7);
      _v#write(oprot);
      oprot#writeFieldEnd
    );
    (match _bools with None -> () | Some _v -> 
      oprot#writeFieldBegin("bools",Protocol.T_LIST,8);
      oprot#writeListBegin(Protocol.T_BOOL,List.length _v);
      List.iter (fun _iter17 ->         oprot#writeBool(_iter17);
      ) _v;
      oprot#writeListEnd;
      oprot#writeFieldEnd
    );
    oprot#writeFieldStop;
    oprot#writeStructEnd
end
let rec read_testStruct (iprot : Protocol.t) =
  let _str18 = new testStruct in
    ignore(iprot#readStructBegin);
    (try while true do
        let (_,_t19,_id20) = iprot#readFieldBegin in
        if _t19 = Protocol.T_STOP then
          raise Break
        else ();
        (match _id20 with 
          | 1 -> (if _t19 = Protocol.T_I32 then
              _str18#set_int32 iprot#readI32
            else
              iprot#skip _t19)
          | 2 -> (if _t19 = Protocol.T_I64 then
              _str18#set_long iprot#readI64
            else
              iprot#skip _t19)
          | 3 -> (if _t19 = Protocol.T_BOOL then
              _str18#set_trueVal iprot#readBool
            else
              iprot#skip _t19)
          | 4 -> (if _t19 = Protocol.T_BOOL then
              _str18#set_falseVal iprot#readBool
            else
              iprot#skip _t19)
          | 5 -> (if _t19 = Protocol.T_DOUBLE then
              _str18#set_decimal iprot#readDouble
            else
              iprot#skip _t19)
          | 6 -> (if _t19 = Protocol.T_STRING then
              _str18#set_comment iprot#readString
            else
              iprot#skip _t19)
          | 7 -> (if _t19 = Protocol.T_STRUCT then
              _str18#set_nested (read_nestedStruct iprot)
            else
              iprot#skip _t19)
          | 8 -> (if _t19 = Protocol.T_LIST then
              _str18#set_bools 
                (let (_etype24,_size21) = iprot#readListBegin in
                  let _con25 = (Array.to_list (Array.init _size21 (fun _ -> iprot#readBool))) in
                    iprot#readListEnd; _con25)
            else
              iprot#skip _t19)
          | _ -> iprot#skip _t19);
        iprot#readFieldEnd;
      done; ()
    with Break -> ());
    iprot#readStructEnd;
    _str18

