/*************************************************************************/
/*                                                                       */
/*                               OCamlCC                                 */
/*                                                                       */
/*                    Michel Mauny, Benoit Vaugon                        */
/*                          ENSTA ParisTech                              */
/*                                                                       */
/*    This file is distributed under the terms of the CeCILL license.    */
/*    See file ../LICENSE-en.                                            */
/*                                                                       */
/*************************************************************************/

#include "mlvalues.h"
#include "prims.h"

extern value caml_alloc_dummy(value size);
extern value caml_alloc_dummy_float(value size);
extern value caml_update_dummy(value dummy, value newval);
extern value caml_array_get_addr(value array, value index);
extern value caml_array_get_float(value array, value index);
extern value caml_array_get(value array, value index);
extern value caml_array_set_addr(value array, value index, value newval);
extern value caml_array_set_float(value array, value index, value newval);
extern value caml_array_set(value array, value index, value newval);
extern value caml_array_unsafe_get_float(value array, value index);
extern value caml_array_unsafe_get(value array, value index);
extern value caml_array_unsafe_set_addr(value array, value index, value newval);
extern value caml_array_unsafe_set_float(value array, value index,
                                         value newval);
extern value caml_array_unsafe_set(value array, value index, value newval);
extern value caml_make_vect(value len, value init);
extern value caml_make_array(value init);
extern value caml_array_blit(value a1, value ofs1, value a2, value ofs2,
                             value n);
extern value caml_array_sub(value a, value ofs, value len);
extern value caml_array_append(value a1, value a2);
extern value caml_array_concat(value al);
extern value caml_compare(value v1, value v2);
extern value caml_equal(value v1, value v2);
extern value caml_notequal(value v1, value v2);
extern value caml_lessthan(value v1, value v2);
extern value caml_lessequal(value v1, value v2);
extern value caml_greaterthan(value v1, value v2);
extern value caml_greaterequal(value v1, value v2);
extern value caml_output_value(value vchan, value v, value flags);
extern value caml_output_value_to_string(value v, value flags);
extern value caml_output_value_to_buffer(value buf, value ofs, value len,
                                         value v, value flags);
extern value caml_format_float(value fmt, value arg);
extern value caml_float_of_string(value vs);
extern value caml_int_of_float(value f);
extern value caml_float_of_int(value n);
extern value caml_neg_float(value f);
extern value caml_abs_float(value f);
extern value caml_add_float(value f, value g);
extern value caml_sub_float(value f, value g);
extern value caml_mul_float(value f, value g);
extern value caml_div_float(value f, value g);
extern value caml_exp_float(value f);
extern value caml_floor_float(value f);
extern value caml_fmod_float(value f1, value f2);
extern value caml_frexp_float(value f);
extern value caml_ldexp_float(value f, value i);
extern value caml_log_float(value f);
extern value caml_log10_float(value f);
extern value caml_modf_float(value f);
extern value caml_sqrt_float(value f);
extern value caml_power_float(value f, value g);
extern value caml_sin_float(value f);
extern value caml_sinh_float(value f);
extern value caml_cos_float(value f);
extern value caml_cosh_float(value f);
extern value caml_tan_float(value f);
extern value caml_tanh_float(value f);
extern value caml_asin_float(value f);
extern value caml_acos_float(value f);
extern value caml_atan_float(value f);
extern value caml_atan2_float(value f, value g);
extern value caml_ceil_float(value f);
extern value caml_hypot_float(value f, value g);
extern value caml_expm1_float(value f);
extern value caml_log1p_float(value f);
extern value caml_copysign_float(value f, value g);
extern value caml_eq_float(value f, value g);
extern value caml_neq_float(value f, value g);
extern value caml_le_float(value f, value g);
extern value caml_lt_float(value f, value g);
extern value caml_ge_float(value f, value g);
extern value caml_gt_float(value f, value g);
extern value caml_float_compare(value f, value g);
extern value caml_classify_float(value vd);
extern value caml_gc_stat(value v);
extern value caml_gc_quick_stat(value v);
extern value caml_gc_counters(value v);
extern value caml_gc_get(value v);
extern value caml_gc_set(value v);
extern value caml_gc_minor(value v);
extern value caml_gc_major(value v);
extern value caml_gc_full_major(value v);
extern value caml_gc_major_slice(value v);
extern value caml_gc_compaction(value v);
extern value caml_hash(value count, value limit, value seed, value obj);
extern value caml_hash_univ_param(value count, value limit, value obj);
extern value caml_input_value(value vchan);
extern value caml_input_value_from_string(value str, value ofs);
extern value caml_marshal_data_size(value buff, value ofs);
extern value caml_int_compare(value v1, value v2);
extern value caml_int_of_string(value s);
extern value caml_format_int(value fmt, value arg);
extern value caml_int32_neg(value v);
extern value caml_int32_add(value v1, value v2);
extern value caml_int32_sub(value v1, value v2);
extern value caml_int32_mul(value v1, value v2);
extern value caml_int32_div(value v1, value v2);
extern value caml_int32_mod(value v1, value v2);
extern value caml_int32_and(value v1, value v2);
extern value caml_int32_or(value v1, value v2);
extern value caml_int32_xor(value v1, value v2);
extern value caml_int32_shift_left(value v1, value v2);
extern value caml_int32_shift_right(value v1, value v2);
extern value caml_int32_shift_right_unsigned(value v1, value v2);
extern value caml_int32_of_int(value v);
extern value caml_int32_to_int(value v);
extern value caml_int32_of_float(value v);
extern value caml_int32_to_float(value v);
extern value caml_int32_compare(value v1, value v2);
extern value caml_int32_format(value fmt, value arg);
extern value caml_int32_of_string(value s);
extern value caml_int32_bits_of_float(value vd);
extern value caml_int32_float_of_bits(value vi);
extern value caml_int64_neg(value v);
extern value caml_int64_add(value v1, value v2);
extern value caml_int64_sub(value v1, value v2);
extern value caml_int64_mul(value v1, value v2);
extern value caml_int64_div(value v1, value v2);
extern value caml_int64_mod(value v1, value v2);
extern value caml_int64_and(value v1, value v2);
extern value caml_int64_or(value v1, value v2);
extern value caml_int64_xor(value v1, value v2);
extern value caml_int64_shift_left(value v1, value v2);
extern value caml_int64_shift_right(value v1, value v2);
extern value caml_int64_shift_right_unsigned(value v1, value v2);
extern value caml_int64_of_int(value v);
extern value caml_int64_to_int(value v);
extern value caml_int64_of_float(value v);
extern value caml_int64_to_float(value v);
extern value caml_int64_of_int32(value v);
extern value caml_int64_to_int32(value v);
extern value caml_int64_of_nativeint(value v);
extern value caml_int64_to_nativeint(value v);
extern value caml_int64_compare(value v1, value v2);
extern value caml_int64_format(value fmt, value arg);
extern value caml_int64_of_string(value s);
extern value caml_int64_bits_of_float(value vd);
extern value caml_int64_float_of_bits(value vi);
extern value caml_nativeint_neg(value v);
extern value caml_nativeint_add(value v1, value v2);
extern value caml_nativeint_sub(value v1, value v2);
extern value caml_nativeint_mul(value v1, value v2);
extern value caml_nativeint_div(value v1, value v2);
extern value caml_nativeint_mod(value v1, value v2);
extern value caml_nativeint_and(value v1, value v2);
extern value caml_nativeint_or(value v1, value v2);
extern value caml_nativeint_xor(value v1, value v2);
extern value caml_nativeint_shift_left(value v1, value v2);
extern value caml_nativeint_shift_right(value v1, value v2);
extern value caml_nativeint_shift_right_unsigned(value v1, value v2);
extern value caml_nativeint_of_int(value v);
extern value caml_nativeint_to_int(value v);
extern value caml_nativeint_of_float(value v);
extern value caml_nativeint_to_float(value v);
extern value caml_nativeint_of_int32(value v);
extern value caml_nativeint_to_int32(value v);
extern value caml_nativeint_compare(value v1, value v2);
extern value caml_nativeint_format(value fmt, value arg);
extern value caml_nativeint_of_string(value s);
extern value caml_ml_open_descriptor_in(value fd);
extern value caml_ml_open_descriptor_out(value fd);
extern value caml_ml_out_channels_list(value unit);
extern value caml_channel_descriptor(value vchannel);
extern value caml_ml_close_channel(value vchannel);
extern value caml_ml_channel_size(value vchannel);
extern value caml_ml_channel_size_64(value vchannel);
extern value caml_ml_set_binary_mode(value vchannel, value mode);
extern value caml_ml_flush_partial(value vchannel);
extern value caml_ml_flush(value vchannel);
extern value caml_ml_output_char(value vchannel, value ch);
extern value caml_ml_output_int(value vchannel, value w);
extern value caml_ml_output_partial(value vchannel, value buff, value start,
                                    value length);
extern value caml_ml_output(value vchannel, value buff, value start,
                            value length);
extern value caml_ml_seek_out(value vchannel, value pos);
extern value caml_ml_seek_out_64(value vchannel, value pos);
extern value caml_ml_pos_out(value vchannel);
extern value caml_ml_pos_out_64(value vchannel);
extern value caml_ml_input_char(value vchannel);
extern value caml_ml_input_int(value vchannel);
extern value caml_ml_input(value vchannel, value buff, value vstart,
                           value vlength);
extern value caml_ml_seek_in(value vchannel, value pos);
extern value caml_ml_seek_in_64(value val, value pos);
extern value caml_ml_pos_in(value vchannel);
extern value caml_ml_pos_in_64(value vchannel);
extern value caml_ml_input_scan_line(value vchannel);
extern value caml_lex_engine(value tbl, value start_state, value lexbuf);
extern value caml_new_lex_engine(value tbl, value start_state, value lexbuf);
extern value caml_md5_string(value str, value ofs, value len);
extern value caml_md5_chan(value vchan, value len);
extern value caml_get_global_data(value unit);
extern value caml_get_section_table(value unit);
extern value caml_reify_bytecode(value prog, value len);
extern value caml_register_code_fragment(value prog, value len, value digest);
extern value caml_realloc_global(value unit);
extern value caml_get_current_environment(value unit);
extern value caml_invoke_traced_function(value codeptr, value env, value arg);
extern value caml_static_alloc(value size);
extern value caml_static_free(value blk);
extern value caml_static_release_bytecode(value blk, value size);
extern value caml_static_resize(value blk, value new_size);
extern value caml_obj_is_block(value arg);
extern value caml_obj_tag(value arg);
extern value caml_obj_set_tag(value arg, value new_tag);
extern value caml_obj_block(value tag, value size);
extern value caml_obj_dup(value arg);
extern value caml_obj_truncate(value v, value newsize);
extern value caml_obj_add_offset(value v, value offset);
extern value caml_lazy_follow_forward(value v);
extern value caml_lazy_make_forward(value v);
extern value caml_get_public_method(value obj, value tag);
extern value caml_parse_engine(value tables, value env, value cmd, value arg);
extern value caml_set_parser_trace(value flag);
extern value caml_install_signal_handler(value signal_number, value action);
extern value caml_ml_string_length(value s);
extern value caml_create_string(value len);
extern value caml_string_get(value str, value index);
extern value caml_string_set(value str, value index, value newval);
extern value caml_string_equal(value s1, value s2);
extern value caml_string_notequal(value s1, value s2);
extern value caml_string_compare(value s1, value s2);
extern value caml_string_lessthan(value s1, value s2);
extern value caml_string_lessequal(value s1, value s2);
extern value caml_string_greaterthan(value s1, value s2);
extern value caml_string_greaterequal(value s1, value s2);
extern value caml_blit_string(value s1, value ofs1, value s2, value ofs2,
                              value n);
extern value caml_fill_string(value s, value offset, value len, value init);
extern value caml_is_printable(value chr);
extern value caml_bitvect_test(value bv, value n);
extern value caml_sys_exit(value retcode);
extern value caml_sys_open(value path, value vflags, value vperm);
extern value caml_sys_close(value fd);
extern value caml_sys_file_exists(value name);
extern value caml_sys_is_directory(value name);
extern value caml_sys_remove(value name);
extern value caml_sys_rename(value oldname, value newname);
extern value caml_sys_chdir(value dirname);
extern value caml_sys_getcwd(value unit);
extern value caml_sys_getenv(value var);
extern value caml_sys_get_argv(value unit);
extern value caml_sys_system_command(value command);
extern value caml_sys_time(value unit);
extern value caml_sys_random_seed(value unit);
extern value caml_sys_get_config(value unit);
extern value caml_sys_read_directory(value path);
extern value caml_terminfo_setup(value vchan);
extern value caml_terminfo_backup(value vlines);
extern value caml_terminfo_standout(value start);
extern value caml_terminfo_resume(value vlines);
extern value caml_register_named_value(value vname, value val);
extern value caml_weak_create(value len);
extern value caml_weak_set(value ar, value n, value el);
extern value caml_weak_get(value ar, value n);
extern value caml_weak_get_copy(value ar, value n);
extern value caml_weak_check(value ar, value n);
extern value caml_weak_blit(value ars, value ofs, value ard, value ofd,
                            value len);
extern value caml_final_register(value f, value v);
extern value caml_final_release(value unit);
extern value caml_ensure_stack_capacity(value required_space);
extern value caml_dynlink_open_lib(value mode, value filename);
extern value caml_dynlink_close_lib(value handle);
extern value caml_dynlink_lookup_symbol(value handle, value symbolname);
extern value caml_dynlink_add_primitive(value handle);
extern value caml_dynlink_get_current_libs(value unit);
extern value caml_record_backtrace(value vflag);
extern value caml_backtrace_status(value vunit);
extern value caml_get_exception_backtrace(value unit);

c_primitive caml_builtin_cprim[] = {
  (c_primitive) caml_alloc_dummy,
  (c_primitive) caml_alloc_dummy_float,
  (c_primitive) caml_update_dummy,
  (c_primitive) caml_array_get_addr,
  (c_primitive) caml_array_get_float,
  (c_primitive) caml_array_get,
  (c_primitive) caml_array_set_addr,
  (c_primitive) caml_array_set_float,
  (c_primitive) caml_array_set,
  (c_primitive) caml_array_unsafe_get_float,
  (c_primitive) caml_array_unsafe_get,
  (c_primitive) caml_array_unsafe_set_addr,
  (c_primitive) caml_array_unsafe_set_float,
  (c_primitive) caml_array_unsafe_set,
  (c_primitive) caml_make_vect,
  (c_primitive) caml_make_array,
  (c_primitive) caml_array_blit,
  (c_primitive) caml_array_sub,
  (c_primitive) caml_array_append,
  (c_primitive) caml_array_concat,
  (c_primitive) caml_compare,
  (c_primitive) caml_equal,
  (c_primitive) caml_notequal,
  (c_primitive) caml_lessthan,
  (c_primitive) caml_lessequal,
  (c_primitive) caml_greaterthan,
  (c_primitive) caml_greaterequal,
  (c_primitive) caml_output_value,
  (c_primitive) caml_output_value_to_string,
  (c_primitive) caml_output_value_to_buffer,
  (c_primitive) caml_format_float,
  (c_primitive) caml_float_of_string,
  (c_primitive) caml_int_of_float,
  (c_primitive) caml_float_of_int,
  (c_primitive) caml_neg_float,
  (c_primitive) caml_abs_float,
  (c_primitive) caml_add_float,
  (c_primitive) caml_sub_float,
  (c_primitive) caml_mul_float,
  (c_primitive) caml_div_float,
  (c_primitive) caml_exp_float,
  (c_primitive) caml_floor_float,
  (c_primitive) caml_fmod_float,
  (c_primitive) caml_frexp_float,
  (c_primitive) caml_ldexp_float,
  (c_primitive) caml_log_float,
  (c_primitive) caml_log10_float,
  (c_primitive) caml_modf_float,
  (c_primitive) caml_sqrt_float,
  (c_primitive) caml_power_float,
  (c_primitive) caml_sin_float,
  (c_primitive) caml_sinh_float,
  (c_primitive) caml_cos_float,
  (c_primitive) caml_cosh_float,
  (c_primitive) caml_tan_float,
  (c_primitive) caml_tanh_float,
  (c_primitive) caml_asin_float,
  (c_primitive) caml_acos_float,
  (c_primitive) caml_atan_float,
  (c_primitive) caml_atan2_float,
  (c_primitive) caml_ceil_float,
  (c_primitive) caml_hypot_float,
  (c_primitive) caml_expm1_float,
  (c_primitive) caml_log1p_float,
  (c_primitive) caml_copysign_float,
  (c_primitive) caml_eq_float,
  (c_primitive) caml_neq_float,
  (c_primitive) caml_le_float,
  (c_primitive) caml_lt_float,
  (c_primitive) caml_ge_float,
  (c_primitive) caml_gt_float,
  (c_primitive) caml_float_compare,
  (c_primitive) caml_classify_float,
  (c_primitive) caml_gc_stat,
  (c_primitive) caml_gc_quick_stat,
  (c_primitive) caml_gc_counters,
  (c_primitive) caml_gc_get,
  (c_primitive) caml_gc_set,
  (c_primitive) caml_gc_minor,
  (c_primitive) caml_gc_major,
  (c_primitive) caml_gc_full_major,
  (c_primitive) caml_gc_major_slice,
  (c_primitive) caml_gc_compaction,
  (c_primitive) caml_hash,
  (c_primitive) caml_hash_univ_param,
  (c_primitive) caml_input_value,
  (c_primitive) caml_input_value_from_string,
  (c_primitive) caml_marshal_data_size,
  (c_primitive) caml_int_compare,
  (c_primitive) caml_int_of_string,
  (c_primitive) caml_format_int,
  (c_primitive) caml_int32_neg,
  (c_primitive) caml_int32_add,
  (c_primitive) caml_int32_sub,
  (c_primitive) caml_int32_mul,
  (c_primitive) caml_int32_div,
  (c_primitive) caml_int32_mod,
  (c_primitive) caml_int32_and,
  (c_primitive) caml_int32_or,
  (c_primitive) caml_int32_xor,
  (c_primitive) caml_int32_shift_left,
  (c_primitive) caml_int32_shift_right,
  (c_primitive) caml_int32_shift_right_unsigned,
  (c_primitive) caml_int32_of_int,
  (c_primitive) caml_int32_to_int,
  (c_primitive) caml_int32_of_float,
  (c_primitive) caml_int32_to_float,
  (c_primitive) caml_int32_compare,
  (c_primitive) caml_int32_format,
  (c_primitive) caml_int32_of_string,
  (c_primitive) caml_int32_bits_of_float,
  (c_primitive) caml_int32_float_of_bits,
  (c_primitive) caml_int64_neg,
  (c_primitive) caml_int64_add,
  (c_primitive) caml_int64_sub,
  (c_primitive) caml_int64_mul,
  (c_primitive) caml_int64_div,
  (c_primitive) caml_int64_mod,
  (c_primitive) caml_int64_and,
  (c_primitive) caml_int64_or,
  (c_primitive) caml_int64_xor,
  (c_primitive) caml_int64_shift_left,
  (c_primitive) caml_int64_shift_right,
  (c_primitive) caml_int64_shift_right_unsigned,
  (c_primitive) caml_int64_of_int,
  (c_primitive) caml_int64_to_int,
  (c_primitive) caml_int64_of_float,
  (c_primitive) caml_int64_to_float,
  (c_primitive) caml_int64_of_int32,
  (c_primitive) caml_int64_to_int32,
  (c_primitive) caml_int64_of_nativeint,
  (c_primitive) caml_int64_to_nativeint,
  (c_primitive) caml_int64_compare,
  (c_primitive) caml_int64_format,
  (c_primitive) caml_int64_of_string,
  (c_primitive) caml_int64_bits_of_float,
  (c_primitive) caml_int64_float_of_bits,
  (c_primitive) caml_nativeint_neg,
  (c_primitive) caml_nativeint_add,
  (c_primitive) caml_nativeint_sub,
  (c_primitive) caml_nativeint_mul,
  (c_primitive) caml_nativeint_div,
  (c_primitive) caml_nativeint_mod,
  (c_primitive) caml_nativeint_and,
  (c_primitive) caml_nativeint_or,
  (c_primitive) caml_nativeint_xor,
  (c_primitive) caml_nativeint_shift_left,
  (c_primitive) caml_nativeint_shift_right,
  (c_primitive) caml_nativeint_shift_right_unsigned,
  (c_primitive) caml_nativeint_of_int,
  (c_primitive) caml_nativeint_to_int,
  (c_primitive) caml_nativeint_of_float,
  (c_primitive) caml_nativeint_to_float,
  (c_primitive) caml_nativeint_of_int32,
  (c_primitive) caml_nativeint_to_int32,
  (c_primitive) caml_nativeint_compare,
  (c_primitive) caml_nativeint_format,
  (c_primitive) caml_nativeint_of_string,
  (c_primitive) caml_ml_open_descriptor_in,
  (c_primitive) caml_ml_open_descriptor_out,
  (c_primitive) caml_ml_out_channels_list,
  (c_primitive) caml_channel_descriptor,
  (c_primitive) caml_ml_close_channel,
  (c_primitive) caml_ml_channel_size,
  (c_primitive) caml_ml_channel_size_64,
  (c_primitive) caml_ml_set_binary_mode,
  (c_primitive) caml_ml_flush_partial,
  (c_primitive) caml_ml_flush,
  (c_primitive) caml_ml_output_char,
  (c_primitive) caml_ml_output_int,
  (c_primitive) caml_ml_output_partial,
  (c_primitive) caml_ml_output,
  (c_primitive) caml_ml_seek_out,
  (c_primitive) caml_ml_seek_out_64,
  (c_primitive) caml_ml_pos_out,
  (c_primitive) caml_ml_pos_out_64,
  (c_primitive) caml_ml_input_char,
  (c_primitive) caml_ml_input_int,
  (c_primitive) caml_ml_input,
  (c_primitive) caml_ml_seek_in,
  (c_primitive) caml_ml_seek_in_64,
  (c_primitive) caml_ml_pos_in,
  (c_primitive) caml_ml_pos_in_64,
  (c_primitive) caml_ml_input_scan_line,
  (c_primitive) caml_lex_engine,
  (c_primitive) caml_new_lex_engine,
  (c_primitive) caml_md5_string,
  (c_primitive) caml_md5_chan,
  (c_primitive) caml_get_global_data,
  (c_primitive) caml_get_section_table,
  (c_primitive) caml_reify_bytecode,
  (c_primitive) caml_register_code_fragment,
  (c_primitive) caml_realloc_global,
  (c_primitive) caml_get_current_environment,
  (c_primitive) caml_invoke_traced_function,
  (c_primitive) caml_static_alloc,
  (c_primitive) caml_static_free,
  (c_primitive) caml_static_release_bytecode,
  (c_primitive) caml_static_resize,
  (c_primitive) caml_obj_is_block,
  (c_primitive) caml_obj_tag,
  (c_primitive) caml_obj_set_tag,
  (c_primitive) caml_obj_block,
  (c_primitive) caml_obj_dup,
  (c_primitive) caml_obj_truncate,
  (c_primitive) caml_obj_add_offset,
  (c_primitive) caml_lazy_follow_forward,
  (c_primitive) caml_lazy_make_forward,
  (c_primitive) caml_get_public_method,
  (c_primitive) caml_parse_engine,
  (c_primitive) caml_set_parser_trace,
  (c_primitive) caml_install_signal_handler,
  (c_primitive) caml_ml_string_length,
  (c_primitive) caml_create_string,
  (c_primitive) caml_string_get,
  (c_primitive) caml_string_set,
  (c_primitive) caml_string_equal,
  (c_primitive) caml_string_notequal,
  (c_primitive) caml_string_compare,
  (c_primitive) caml_string_lessthan,
  (c_primitive) caml_string_lessequal,
  (c_primitive) caml_string_greaterthan,
  (c_primitive) caml_string_greaterequal,
  (c_primitive) caml_blit_string,
  (c_primitive) caml_fill_string,
  (c_primitive) caml_is_printable,
  (c_primitive) caml_bitvect_test,
  (c_primitive) caml_sys_exit,
  (c_primitive) caml_sys_open,
  (c_primitive) caml_sys_close,
  (c_primitive) caml_sys_file_exists,
  (c_primitive) caml_sys_is_directory,
  (c_primitive) caml_sys_remove,
  (c_primitive) caml_sys_rename,
  (c_primitive) caml_sys_chdir,
  (c_primitive) caml_sys_getcwd,
  (c_primitive) caml_sys_getenv,
  (c_primitive) caml_sys_get_argv,
  (c_primitive) caml_sys_system_command,
  (c_primitive) caml_sys_time,
  (c_primitive) caml_sys_random_seed,
  (c_primitive) caml_sys_get_config,
  (c_primitive) caml_sys_read_directory,
  (c_primitive) caml_terminfo_setup,
  (c_primitive) caml_terminfo_backup,
  (c_primitive) caml_terminfo_standout,
  (c_primitive) caml_terminfo_resume,
  (c_primitive) caml_register_named_value,
  (c_primitive) caml_weak_create,
  (c_primitive) caml_weak_set,
  (c_primitive) caml_weak_get,
  (c_primitive) caml_weak_get_copy,
  (c_primitive) caml_weak_check,
  (c_primitive) caml_weak_blit,
  (c_primitive) caml_final_register,
  (c_primitive) caml_final_release,
  (c_primitive) caml_ensure_stack_capacity,
  (c_primitive) caml_dynlink_open_lib,
  (c_primitive) caml_dynlink_close_lib,
  (c_primitive) caml_dynlink_lookup_symbol,
  (c_primitive) caml_dynlink_add_primitive,
  (c_primitive) caml_dynlink_get_current_libs,
  (c_primitive) caml_record_backtrace,
  (c_primitive) caml_backtrace_status,
  (c_primitive) caml_get_exception_backtrace,
  0
};

const char * caml_names_of_builtin_cprim[] = {
  "caml_alloc_dummy",
  "caml_alloc_dummy_float",
  "caml_update_dummy",
  "caml_array_get_addr",
  "caml_array_get_float",
  "caml_array_get",
  "caml_array_set_addr",
  "caml_array_set_float",
  "caml_array_set",
  "caml_array_unsafe_get_float",
  "caml_array_unsafe_get",
  "caml_array_unsafe_set_addr",
  "caml_array_unsafe_set_float",
  "caml_array_unsafe_set",
  "caml_make_vect",
  "caml_make_array",
  "caml_array_blit",
  "caml_array_sub",
  "caml_array_append",
  "caml_array_concat",
  "caml_compare",
  "caml_equal",
  "caml_notequal",
  "caml_lessthan",
  "caml_lessequal",
  "caml_greaterthan",
  "caml_greaterequal",
  "caml_output_value",
  "caml_output_value_to_string",
  "caml_output_value_to_buffer",
  "caml_format_float",
  "caml_float_of_string",
  "caml_int_of_float",
  "caml_float_of_int",
  "caml_neg_float",
  "caml_abs_float",
  "caml_add_float",
  "caml_sub_float",
  "caml_mul_float",
  "caml_div_float",
  "caml_exp_float",
  "caml_floor_float",
  "caml_fmod_float",
  "caml_frexp_float",
  "caml_ldexp_float",
  "caml_log_float",
  "caml_log10_float",
  "caml_modf_float",
  "caml_sqrt_float",
  "caml_power_float",
  "caml_sin_float",
  "caml_sinh_float",
  "caml_cos_float",
  "caml_cosh_float",
  "caml_tan_float",
  "caml_tanh_float",
  "caml_asin_float",
  "caml_acos_float",
  "caml_atan_float",
  "caml_atan2_float",
  "caml_ceil_float",
  "caml_hypot_float",
  "caml_expm1_float",
  "caml_log1p_float",
  "caml_copysign_float",
  "caml_eq_float",
  "caml_neq_float",
  "caml_le_float",
  "caml_lt_float",
  "caml_ge_float",
  "caml_gt_float",
  "caml_float_compare",
  "caml_classify_float",
  "caml_gc_stat",
  "caml_gc_quick_stat",
  "caml_gc_counters",
  "caml_gc_get",
  "caml_gc_set",
  "caml_gc_minor",
  "caml_gc_major",
  "caml_gc_full_major",
  "caml_gc_major_slice",
  "caml_gc_compaction",
  "caml_hash",
  "caml_hash_univ_param",
  "caml_input_value",
  "caml_input_value_from_string",
  "caml_marshal_data_size",
  "caml_int_compare",
  "caml_int_of_string",
  "caml_format_int",
  "caml_int32_neg",
  "caml_int32_add",
  "caml_int32_sub",
  "caml_int32_mul",
  "caml_int32_div",
  "caml_int32_mod",
  "caml_int32_and",
  "caml_int32_or",
  "caml_int32_xor",
  "caml_int32_shift_left",
  "caml_int32_shift_right",
  "caml_int32_shift_right_unsigned",
  "caml_int32_of_int",
  "caml_int32_to_int",
  "caml_int32_of_float",
  "caml_int32_to_float",
  "caml_int32_compare",
  "caml_int32_format",
  "caml_int32_of_string",
  "caml_int32_bits_of_float",
  "caml_int32_float_of_bits",
  "caml_int64_neg",
  "caml_int64_add",
  "caml_int64_sub",
  "caml_int64_mul",
  "caml_int64_div",
  "caml_int64_mod",
  "caml_int64_and",
  "caml_int64_or",
  "caml_int64_xor",
  "caml_int64_shift_left",
  "caml_int64_shift_right",
  "caml_int64_shift_right_unsigned",
  "caml_int64_of_int",
  "caml_int64_to_int",
  "caml_int64_of_float",
  "caml_int64_to_float",
  "caml_int64_of_int32",
  "caml_int64_to_int32",
  "caml_int64_of_nativeint",
  "caml_int64_to_nativeint",
  "caml_int64_compare",
  "caml_int64_format",
  "caml_int64_of_string",
  "caml_int64_bits_of_float",
  "caml_int64_float_of_bits",
  "caml_nativeint_neg",
  "caml_nativeint_add",
  "caml_nativeint_sub",
  "caml_nativeint_mul",
  "caml_nativeint_div",
  "caml_nativeint_mod",
  "caml_nativeint_and",
  "caml_nativeint_or",
  "caml_nativeint_xor",
  "caml_nativeint_shift_left",
  "caml_nativeint_shift_right",
  "caml_nativeint_shift_right_unsigned",
  "caml_nativeint_of_int",
  "caml_nativeint_to_int",
  "caml_nativeint_of_float",
  "caml_nativeint_to_float",
  "caml_nativeint_of_int32",
  "caml_nativeint_to_int32",
  "caml_nativeint_compare",
  "caml_nativeint_format",
  "caml_nativeint_of_string",
  "caml_ml_open_descriptor_in",
  "caml_ml_open_descriptor_out",
  "caml_ml_out_channels_list",
  "caml_channel_descriptor",
  "caml_ml_close_channel",
  "caml_ml_channel_size",
  "caml_ml_channel_size_64",
  "caml_ml_set_binary_mode",
  "caml_ml_flush_partial",
  "caml_ml_flush",
  "caml_ml_output_char",
  "caml_ml_output_int",
  "caml_ml_output_partial",
  "caml_ml_output",
  "caml_ml_seek_out",
  "caml_ml_seek_out_64",
  "caml_ml_pos_out",
  "caml_ml_pos_out_64",
  "caml_ml_input_char",
  "caml_ml_input_int",
  "caml_ml_input",
  "caml_ml_seek_in",
  "caml_ml_seek_in_64",
  "caml_ml_pos_in",
  "caml_ml_pos_in_64",
  "caml_ml_input_scan_line",
  "caml_lex_engine",
  "caml_new_lex_engine",
  "caml_md5_string",
  "caml_md5_chan",
  "caml_get_global_data",
  "caml_get_section_table",
  "caml_reify_bytecode",
  "caml_register_code_fragment",
  "caml_realloc_global",
  "caml_get_current_environment",
  "caml_invoke_traced_function",
  "caml_static_alloc",
  "caml_static_free",
  "caml_static_release_bytecode",
  "caml_static_resize",
  "caml_obj_is_block",
  "caml_obj_tag",
  "caml_obj_set_tag",
  "caml_obj_block",
  "caml_obj_dup",
  "caml_obj_truncate",
  "caml_obj_add_offset",
  "caml_lazy_follow_forward",
  "caml_lazy_make_forward",
  "caml_get_public_method",
  "caml_parse_engine",
  "caml_set_parser_trace",
  "caml_install_signal_handler",
  "caml_ml_string_length",
  "caml_create_string",
  "caml_string_get",
  "caml_string_set",
  "caml_string_equal",
  "caml_string_notequal",
  "caml_string_compare",
  "caml_string_lessthan",
  "caml_string_lessequal",
  "caml_string_greaterthan",
  "caml_string_greaterequal",
  "caml_blit_string",
  "caml_fill_string",
  "caml_is_printable",
  "caml_bitvect_test",
  "caml_sys_exit",
  "caml_sys_open",
  "caml_sys_close",
  "caml_sys_file_exists",
  "caml_sys_is_directory",
  "caml_sys_remove",
  "caml_sys_rename",
  "caml_sys_chdir",
  "caml_sys_getcwd",
  "caml_sys_getenv",
  "caml_sys_get_argv",
  "caml_sys_system_command",
  "caml_sys_time",
  "caml_sys_random_seed",
  "caml_sys_get_config",
  "caml_sys_read_directory",
  "caml_terminfo_setup",
  "caml_terminfo_backup",
  "caml_terminfo_standout",
  "caml_terminfo_resume",
  "caml_register_named_value",
  "caml_weak_create",
  "caml_weak_set",
  "caml_weak_get",
  "caml_weak_get_copy",
  "caml_weak_check",
  "caml_weak_blit",
  "caml_final_register",
  "caml_final_release",
  "caml_ensure_stack_capacity",
  "caml_dynlink_open_lib",
  "caml_dynlink_close_lib",
  "caml_dynlink_lookup_symbol",
  "caml_dynlink_add_primitive",
  "caml_dynlink_get_current_libs",
  "caml_record_backtrace",
  "caml_backtrace_status",
  "caml_get_exception_backtrace",
  0
};
