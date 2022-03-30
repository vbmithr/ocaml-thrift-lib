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
module T = Transport

class t ((i ,o) : (Buffer.t * Buffer.t)) =
object
  val mutable _opened = true
  val mutable _current_idx_input = 0

  inherit Transport.t
  method isOpen = _opened
  method opn = ()
  method close = _opened <- false
  method read buf off len =
    let input_buf_length = Buffer.length i in
    if len + _current_idx_input > input_buf_length then
      raise (T.E (T.UNKNOWN, "Input buffer overflow."))
    else
    for idx = 0 to len - 1 do
      Bytes.set buf (off + idx) (Buffer.nth i (_current_idx_input + idx))
    done;
    _current_idx_input <- _current_idx_input + len;
    len

  method write buf off len =
    Buffer.add_bytes o @@ Bytes.sub buf off len

  method write_string buf off len =
    Buffer.add_string o @@ String.sub buf off len

  method flush = ()
end
