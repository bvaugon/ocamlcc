(*************************************************************************)
(*                                                                       *)
(*                               OCamlCC                                 *)
(*                                                                       *)
(*                    Michel Mauny, Benoit Vaugon                        *)
(*                          ENSTA ParisTech                              *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

open Types
open Tools

let parse ic index =
  let (offset, length) =
    try Index.find_section index Prim
    with Not_found -> failwith "prim section not found"
  in
  seek_in ic offset;
  let buf = Buffer.create 16 in
  let rec f i res =
    if i <> length then
      let c = input_char ic in
      if int_of_char c <> 0 then
        begin
          Buffer.add_char buf c;
          f (i + 1) res
        end
      else
        let name = Buffer.contents buf in
        Buffer.clear buf;
        f (i + 1) (name :: res)
    else if Buffer.length buf <> 0 then
      failwith "unexpected end of prim section"
    else
      res
  in
  Array.of_list (List.rev (f 0 []))
;;

let describe =
  let taa = (true, Allocated, [ Allocated ])
  and tai = (true, Allocated, [ Integer ])
  and fai = (false, Allocated, [ Integer ])
  and tau = (true, Allocated, [ Unknown ])
  and tia = (true, Integer, [ Allocated ])
  and fia = (false, Integer, [ Allocated ])
  and tii = (true, Integer, [ Integer ])
  and fii = (false, Integer, [ Integer ])
  and fiu = (false, Integer, [ Unknown ])
  and tua = (true, Unknown, [ Allocated ])
  and tui = (true, Unknown, [ Integer ])
  and tuu = (true, Unknown, [ Unknown ])
  and fuu = (false, Unknown, [ Unknown ])
  and taaa = (true, Allocated, [ Allocated ; Allocated ])
  and taai = (true, Allocated, [ Allocated ; Integer ])
  and faai = (false, Allocated, [ Allocated ; Integer ])
  and taia = (true, Allocated, [ Integer ; Allocated ])
  and taii = (true, Allocated, [ Integer ; Integer ])
  and taiu = (true, Allocated, [ Integer ; Unknown ])
  and tauu = (true, Allocated, [ Unknown ; Unknown ])
  and tiaa = (true, Integer, [ Allocated ; Allocated ])
  and fiaa = (false, Integer, [ Allocated ; Allocated ])
  and tiai = (true, Integer, [ Allocated ; Integer ])
  and fiai = (false, Integer, [ Allocated ; Integer ])
  and tiau = (true, Integer, [ Allocated ; Unknown ])
  and fiau = (false, Integer, [ Allocated ; Unknown ])
  and fiii = (false, Integer, [ Integer ; Integer ])
  and fiuu = (false, Integer, [ Unknown ; Unknown ])
  and tuai = (true, Unknown, [ Allocated ; Integer ])
  and fuai = (false, Unknown, [ Allocated ; Integer ])
  and tuiu = (true, Unknown, [ Integer ; Unknown ])
  and tuui = (true, Unknown, [ Unknown ; Integer ])
  and taaii = (true, Allocated, [ Allocated ; Integer ; Integer ])
  and tiaiu = (true, Integer, [ Allocated ; Integer ; Unknown ])
  and fiaiu = (false, Integer, [ Allocated ; Integer ; Unknown ])
  and tiaui = (true, Integer, [ Allocated ; Unknown ; Integer ])
  and tiauu = (true, Integer, [ Allocated ; Unknown ; Unknown ])
  and tiaia = (true, Integer, [ Allocated ; Integer ; Allocated ])
  and tiaii = (true, Integer, [ Allocated ; Integer ; Integer ])
  and tiiau = (true, Integer, [ Integer ; Allocated ; Unknown ])
  and fiiiu = (false, Integer, [ Integer ; Integer ; Unknown ])
  and tiaaii = (true, Integer, [ Allocated ; Allocated ; Integer ; Integer ])
  and fiaaii = (false, Integer, [ Allocated ; Allocated ; Integer ; Integer ])
  and tiaaiu = (true, Integer, [ Allocated ; Allocated ; Integer ; Unknown ])
  and fiaiii = (false, Integer, [ Allocated ; Integer ; Integer ; Integer ])
  and tiaiaii = (true, Integer,[ Allocated ; Integer ; Allocated ; Integer ; Integer ])
  and fiaiaii = (false, Integer,[ Allocated ; Integer ; Allocated ; Integer ; Integer ])
  and tiaiiuu = (true, Integer, [ Allocated ; Integer ; Integer ; Unknown ; Unknown ])
  in let base = [
       ("caml_alloc_dummy", tai);
       ("caml_alloc_dummy_float", tai);
       ("caml_update_dummy", fiaa);
       ("caml_array_get_addr", tuai);
       ("caml_array_get_float", tuai);
       ("caml_array_get", tuai);
       ("caml_array_set_addr", tiaiu);
       ("caml_array_set_float", tiaiu);
       ("caml_array_set", tiaiu);
       ("caml_array_unsafe_get_float", tuai);
       ("caml_array_unsafe_get", tuai);
       ("caml_array_unsafe_set_addr", fiaiu);
       ("caml_array_unsafe_set_float", fiaiu);
       ("caml_array_unsafe_set", fiaiu);
       ("caml_make_vect", taiu);
       ("caml_make_array", taa);
       ("caml_compare", fiuu);
       ("caml_equal", fiuu);
       ("caml_notequal", fiuu);
       ("caml_lessthan", fiuu);
       ("caml_lessequal", fiuu);
       ("caml_greaterthan", fiuu);
       ("caml_greaterequal", fiuu);
       ("caml_output_value", tiauu);
       ("caml_output_value_to_string", tauu);
       ("caml_output_value_to_buffer", tiaiiuu);
       ("caml_format_float", taaa);
       ("caml_float_of_string", taa);
       ("caml_int_of_float", fia);
       ("caml_float_of_int", tai);
       ("caml_neg_float", taa);
       ("caml_abs_float", taa);
       ("caml_add_float", taaa);
       ("caml_sub_float", taaa);
       ("caml_mul_float", taaa);
       ("caml_div_float", taaa);
       ("caml_exp_float", taa);
       ("caml_floor_float", taa);
       ("caml_fmod_float", taaa);
       ("caml_frexp_float", taa);
       ("caml_ldexp_float", taai);
       ("caml_log_float", taa);
       ("caml_log10_float", taa);
       ("caml_modf_float", taa);
       ("caml_sqrt_float", taa);
       ("caml_power_float", taaa);
       ("caml_sin_float", taa);
       ("caml_sinh_float", taa);
       ("caml_cos_float", taa);
       ("caml_cosh_float", taa);
       ("caml_tan_float", taa);
       ("caml_tanh_float", taa);
       ("caml_asin_float", taa);
       ("caml_acos_float", taa);
       ("caml_atan_float", taa);
       ("caml_atan2_float", taaa);
       ("caml_ceil_float", taa);
       ("caml_expm1_float", taa);
       ("caml_log1p_float", taa);
       ("caml_eq_float", fiaa);
       ("caml_neq_float", fiaa);
       ("caml_le_float", fiaa);
       ("caml_lt_float", fiaa);
       ("caml_ge_float", fiaa);
       ("caml_gt_float", fiaa);
       ("caml_float_compare", fiaa);
       ("caml_classify_float", fia);
       ("caml_gc_stat", tai);
       ("caml_gc_quick_stat", tai);
       ("caml_gc_counters", tai);
       ("caml_gc_get", tai);
       ("caml_gc_set", tia);
       ("caml_gc_minor", tii);
       ("caml_gc_major", tii);
       ("caml_gc_full_major", tii);
       ("caml_gc_major_slice", tii);
       ("caml_gc_compaction", tii);
       ("caml_hash_univ_param", fiiiu);
       ("caml_input_value", tua);
       ("caml_input_value_from_string", tuai);
       ("caml_marshal_data_size", tiai);
       ("caml_int_compare", fiii);
       ("caml_int_of_string", fia);
       ("caml_format_int", taai);
       ("caml_int32_neg", taa);
       ("caml_int32_add", taaa);
       ("caml_int32_sub", taaa);
       ("caml_int32_mul", taaa);
       ("caml_int32_div", taaa);
       ("caml_int32_mod", taaa);
       ("caml_int32_and", taaa);
       ("caml_int32_or", taaa);
       ("caml_int32_xor", taaa);
       ("caml_int32_shift_left", taai);
       ("caml_int32_shift_right", taai);
       ("caml_int32_shift_right_unsigned", taai);
       ("caml_int32_of_int", tai);
       ("caml_int32_to_int", fia);
       ("caml_int32_of_float", taa);
       ("caml_int32_to_float", taa);
       ("caml_int32_compare", fiaa);
       ("caml_int32_format", taaa);
       ("caml_int32_of_string", taa);
       ("caml_int32_bits_of_float", taa);
       ("caml_int32_float_of_bits", taa);
       ("caml_int64_neg", taa);
       ("caml_int64_add", taaa);
       ("caml_int64_sub", taaa);
       ("caml_int64_mul", taaa);
       ("caml_int64_div", taaa);
       ("caml_int64_mod", taaa);
       ("caml_int64_and", taaa);
       ("caml_int64_or", taaa);
       ("caml_int64_xor", taaa);
       ("caml_int64_shift_left", taai);
       ("caml_int64_shift_right", taai);
       ("caml_int64_shift_right_unsigned", taai);
       ("caml_int64_of_int", tai);
       ("caml_int64_to_int", fia);
       ("caml_int64_of_float", taa);
       ("caml_int64_to_float", taa);
       ("caml_int64_of_int32", taa);
       ("caml_int64_to_int32", taa);
       ("caml_int64_of_nativeint", taa);
       ("caml_int64_to_nativeint", taa);
       ("caml_int64_compare", fiaa);
       ("caml_int64_format", taaa);
       ("caml_int64_of_string", taa);
       ("caml_int64_bits_of_float", taa);
       ("caml_int64_float_of_bits", taa);
       ("caml_nativeint_neg", taa);
       ("caml_nativeint_add", taaa);
       ("caml_nativeint_sub", taaa);
       ("caml_nativeint_mul", taaa);
       ("caml_nativeint_div", taaa);
       ("caml_nativeint_mod", taaa);
       ("caml_nativeint_and", taaa);
       ("caml_nativeint_or", taaa);
       ("caml_nativeint_xor", taaa);
       ("caml_nativeint_shift_left", taai);
       ("caml_nativeint_shift_right", taai);
       ("caml_nativeint_shift_right_unsigned", taai);
       ("caml_nativeint_of_int", tai);
       ("caml_nativeint_to_int", fia);
       ("caml_nativeint_of_float", taa);
       ("caml_nativeint_to_float", taa);
       ("caml_nativeint_of_int32", taa);
       ("caml_nativeint_to_int32", taa);
       ("caml_nativeint_compare", fiaa);
       ("caml_nativeint_format", taaa);
       ("caml_nativeint_of_string", taa);
       ("caml_ml_open_descriptor_in", tai);
       ("caml_ml_open_descriptor_out", tai);
       ("caml_ml_out_channels_list", tui);
       ("caml_channel_descriptor", tia);
       ("caml_ml_close_channel", tia);
       ("caml_ml_channel_size", tia);
       ("caml_ml_channel_size_64", taa);
       ("caml_ml_set_binary_mode", tiai);
       ("caml_ml_flush_partial", tia);
       ("caml_ml_flush", tia);
       ("caml_ml_output_char", fiai);
       ("caml_ml_output_int", fiai);
       ("caml_ml_output_partial", fiaaii);
       ("caml_ml_output", fiaaii);
       ("caml_ml_seek_out", tiai);
       ("caml_ml_seek_out_64", tiaa);
       ("caml_ml_pos_out", tia);
       ("caml_ml_pos_out_64", taa);
       ("caml_ml_input_char", fia);
       ("caml_ml_input_int", fia);
       ("caml_ml_input", tiaaii);
       ("caml_ml_seek_in", tiai);
       ("caml_ml_seek_in_64", tiaa);
       ("caml_ml_pos_in", tia);
       ("caml_ml_pos_in_64", taa);
       ("caml_ml_input_scan_line", tia);
       ("caml_lex_engine", tiaia);
       ("caml_new_lex_engine", tiaia);
       ("caml_md5_string", taaii);
       ("caml_md5_chan", taai);
       ("caml_get_global_data", fai);
       ("caml_get_section_table", tai);
       ("caml_reify_bytecode", taii);
       ("caml_realloc_global", tii);
       ("caml_get_current_environment", fai);
       ("caml_invoke_traced_function", tiiau);
       ("caml_static_alloc", fii);
       ("caml_static_free", fii);
       ("caml_static_release_bytecode", fiii);
       ("caml_static_resize", fiii);
       ("caml_obj_is_block", fiu);
       ("caml_obj_tag", fiu);
       ("caml_obj_set_tag", fiai);
       ("caml_obj_block", taii);
       ("caml_obj_dup", tuu);
       ("caml_obj_truncate", tuui);
       ("caml_obj_add_offset", faai);
       ("caml_lazy_follow_forward", fuu);
       ("caml_lazy_make_forward", tau);
       ("caml_get_public_method", fuai);
       ("caml_parse_engine", tiaaiu);
       ("caml_set_parser_trace", fii);
       ("caml_install_signal_handler", tuiu);
       ("caml_ml_string_length", fia);
       ("caml_create_string", tai);
       ("caml_string_get", tiai);
       ("caml_string_set", tiaii);
       ("caml_string_equal", fiaa);
       ("caml_string_notequal", fiaa);
       ("caml_string_compare", fiaa);
       ("caml_string_lessthan", fiaa);
       ("caml_string_lessequal", fiaa);
       ("caml_string_greaterthan", fiaa);
       ("caml_string_greaterequal", fiaa);
       ("caml_blit_string", fiaiaii);
       ("caml_fill_string", fiaiii);
       ("caml_is_printable", fii);
       ("caml_bitvect_test", fiai);
       ("caml_sys_exit", fii);
       ("caml_sys_open", tiaui);
       ("caml_sys_close", fii);
       ("caml_sys_file_exists", fia);
       ("caml_sys_is_directory", tia);
       ("caml_sys_remove", tia);
       ("caml_sys_rename", tiaa);
       ("caml_sys_chdir", tia);
       ("caml_sys_getcwd", tai);
       ("caml_sys_getenv", taa);
       ("caml_sys_get_argv", tai);
       ("caml_sys_system_command", tia);
       ("caml_sys_time", tai);
       ("caml_sys_random_seed", fii);
       ("caml_sys_get_config", tai);
       ("caml_sys_read_directory", taa);
       ("caml_terminfo_setup", tua);
       ("caml_terminfo_backup", fii);
       ("caml_terminfo_standout", fii);
       ("caml_terminfo_resume", fii);
       ("caml_register_named_value", fiau);
       ("caml_weak_create", tai);
       ("caml_weak_set", tiaiu);
       ("caml_weak_get", tuai);
       ("caml_weak_get_copy", tuai);
       ("caml_weak_check", tiai);
       ("caml_weak_blit", tiaiaii);
       ("caml_final_register", tiau);
       ("caml_final_release", fii);
       ("caml_ensure_stack_capacity", fii);
       ("caml_dynlink_open_lib", taia);
       ("caml_dynlink_close_lib", tia);
       ("caml_dynlink_lookup_symbol", taaa);
       ("caml_dynlink_add_primitive", tia);
       ("caml_dynlink_get_current_libs", tai);
       ("caml_record_backtrace", fii);
       ("caml_backtrace_status", fii);
       ("caml_get_exception_backtrace", tui);
     ] in
     let index =
       List.fold_left (fun acc (name, desc) -> SMap.add name desc acc) SMap.empty
         base
     in
     fun name -> try Some (SMap.find name index) with Not_found -> None
;;
