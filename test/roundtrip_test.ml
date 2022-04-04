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

module Testables = struct
  type nested_struct_record =
    { message : string
    }
  [@@deriving show, eq]

  type test_struct_record =
    { comment : string
    ; decimal : float
    ; falseVar : bool
    ; trueVal : bool
    ; int32 : Int32.t
    ; long : Int64.t
    ; nested : nested_struct_record
    ; bools : bool list
    }
  [@@deriving show, eq]

  let nested_struct_to_record nested_struct =
    let message = nested_struct#grab_message in
    { message
    }

  let test_struct_to_record test_struct =
    let comment = test_struct#grab_comment in
    let decimal = test_struct#grab_decimal in
    let falseVar = test_struct#grab_falseVal in
    let trueVal = test_struct#grab_trueVal in
    let int32 = test_struct#grab_int32 in
    let long = test_struct#grab_long in
    let nested = nested_struct_to_record @@ test_struct#grab_nested in
    let bools = test_struct#grab_bools in
    { comment
    ; decimal
    ; falseVar
    ; trueVal
    ; int32
    ; long
    ; nested
    ; bools
    }

  let test_struct = Alcotest.testable pp_test_struct_record equal_test_struct_record
end

module Utils = struct
  let test_transport () =
    let buffer = Buffer.create 1024 in
    new Test_transport.t (buffer, buffer)

  let make_test_cases test_name speed cases case_name test =
    cases
      |> List.map (fun case ->
        Alcotest.test_case (case_name test_name case) speed (test case))
end

let test_round_trip protocol_factory () =
  (* Arrange *)
  let trans = Utils.test_transport () in
  let proto = protocol_factory trans in

  let nested_struct = new Test_types.nestedStruct in
  nested_struct#set_message "Hello world!";

  let test_struct = new Test_types.testStruct in
  test_struct#set_comment "No comment.";
  test_struct#set_decimal 3.14;
  test_struct#set_falseVal false;
  test_struct#set_trueVal true;
  test_struct#set_int32 @@ Int32.of_int 42;
  test_struct#set_long @@ Int64.of_int 42;
  test_struct#set_nested nested_struct;
  test_struct#set_bools [true; false; false; true];

  (* Act *)
  test_struct#write proto;
  let round_tripped = Test_types.read_testStruct proto in
  let round_tripped = Testables.test_struct_to_record round_tripped in
  let test_struct = Testables.test_struct_to_record test_struct in

  (* Assert *)
  Alcotest.check Testables.test_struct
    "Round tripped data structure should be the same as the original"
    test_struct
    round_tripped

module TestReadWriteInt (I : TCompactProtocol.IntSig) = struct
  module RWI = TCompactProtocol.ReadWriteInt(I)
  open RWI

  module Testables = struct
    let pp = fun fmt t -> Format.fprintf fmt "%s" @@ I.to_string t
    let int_testable = Alcotest.testable pp I.equal
  end

  let test_round_trip_varint test_number () =
    let trans = Utils.test_transport () in
    let buffer = Bytes.create 8 in

    write_varint trans test_number;
    let decoded = read_varint trans buffer in

    Alcotest.check Testables.int_testable
      "Round tripped varints should be invariant"
      test_number
      decoded

  let test_round_trip_zig_zag bits test_number () =
    let encoded = to_zig_zag test_number bits in
    let decoded = from_zig_zag encoded in

    Alcotest.check Testables.int_testable
      "Round tripped zig zag should be invariant"
      test_number
      decoded

  let test_cases = [I.zero; I.one; I.max_int; I.min_int; I.of_int 42; I.of_int ~-128]

  let case_naming test_name case = Printf.sprintf "%s (%s)" test_name @@ I.to_string case
end

module TRI   = TestReadWriteInt(TCompactProtocol.Int)
module TRI32 = TestReadWriteInt(Int32)
module TRI64 = TestReadWriteInt(Int64)

let all =
  [ Alcotest.test_case "Round trip a struct using TCompactProtocol" `Quick @@ test_round_trip (fun trans -> new TCompactProtocol.t trans)
  ; Alcotest.test_case "Round trip a struct using TBinaryProtocol" `Quick @@ test_round_trip (fun trans -> new TBinaryProtocol.t trans)
  ]
  @ Utils.make_test_cases "Round trip Int varint" `Quick TRI.test_cases TRI.case_naming TRI.test_round_trip_varint
  @ Utils.make_test_cases "Round trip Int32 varint" `Quick TRI32.test_cases TRI32.case_naming TRI32.test_round_trip_varint
  @ Utils.make_test_cases "Round trip Int64 varint" `Quick TRI64.test_cases TRI64.case_naming TRI64.test_round_trip_varint
  @ (Utils.make_test_cases "Round trip Int zigzag" `Quick TRI.test_cases TRI.case_naming @@ TRI.test_round_trip_zig_zag Sys.int_size)
  @ (Utils.make_test_cases "Round trip Int32 zigzag" `Quick TRI32.test_cases TRI32.case_naming @@ TRI32.test_round_trip_zig_zag 32)
  @ (Utils.make_test_cases "Round trip Int64 zigzag" `Quick TRI64.test_cases TRI64.case_naming @@ TRI64.test_round_trip_zig_zag 64)
