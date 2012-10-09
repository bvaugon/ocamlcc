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

CAMLprim value add_nat(value *argv, int argn);
CAMLprim value blit_nat(value nat1, value ofs1,
                        value nat2, value ofs2,
                        value len);
CAMLprim value caml_ba_blit(value vsrc, value vdst);
CAMLprim value caml_ba_create(value vkind, value vlayout, value vdim);
CAMLprim value caml_ba_dim(value vb, value vn);
CAMLprim value caml_ba_fill(value vb, value vinit);
CAMLprim value caml_ba_get_1(value vb, value vind1);
CAMLprim value caml_ba_get_2(value vb, value vind1, value vind2);
CAMLprim value caml_ba_get_3(value vb, value vind1, value vind2, value vind3);
CAMLprim value caml_ba_get_4(value vb, value vind1, value vind2,
                             value vind3, value vind4);
CAMLprim value caml_ba_get_5(value vb, value vind1, value vind2,
                             value vind3, value vind4, value vind5);
CAMLprim value caml_ba_get_6(value vb, value vind1, value vind2,
                             value vind3, value vind4, value vind5,
                             value vind6);
CAMLprim value caml_ba_get_generic(value vb, value vind);
CAMLprim value caml_ba_init(value unit);
CAMLprim value caml_ba_kind(value vb);
CAMLprim value caml_ba_layout(value vb);
CAMLprim value caml_ba_map_file_bytecode(value * argv, int argn);
CAMLprim value caml_ba_map_file(value vfd, value vkind, value vlayout,
                                value vshared, value vdim, value vstart);
CAMLprim value caml_ba_num_dims(value vb);
CAMLprim value caml_ba_reshape(value vb, value vdim);
CAMLprim value caml_ba_set_1(value vb, value vind1, value newval);
CAMLprim value caml_ba_set_2(value vb, value vind1, value vind2, value newval);
CAMLprim value caml_ba_set_3(value vb, value vind1, value vind2, value vind3,
                             value newval);
CAMLprim value caml_ba_set_4(value vb, value vind1, value vind2,
                             value vind3, value vind4, value newval);
CAMLprim value caml_ba_set_5(value vb, value vind1, value vind2,
                             value vind3, value vind4, value vind5,
                             value newval);
CAMLprim value caml_ba_set_6(value vb, value vind1, value vind2,
                             value vind3, value vind4, value vind5,
                             value vind6, value newval);
CAMLprim value caml_ba_set_generic(value vb, value vind, value newval);
CAMLprim value caml_ba_slice(value vb, value vind);
CAMLprim value caml_ba_sub(value vb, value vofs, value vlen);
CAMLprim value caml_condition_broadcast(value wrapper);
CAMLprim value caml_condition_new(value unit);
CAMLprim value caml_condition_signal(value wrapper);
CAMLprim value caml_condition_wait(value wcond, value wmut);
CAMLprim value caml_gr_blit_image (value i, value x, value y);
CAMLprim value caml_gr_circle(value x,value y,value radius);
CAMLprim value caml_gr_clear_graph(value unit);
CAMLprim value caml_gr_close_graph(value unit);
CAMLprim value caml_gr_create_image(value vw, value vh);
CAMLprim value caml_gr_current_x(value unit);
CAMLprim value caml_gr_current_y(value unit);
CAMLprim value caml_gr_display_mode(value flag);
CAMLprim value caml_gr_draw_arc_nat(value vx, value vy, value vrx, value vry,
                                    value vstart, value vend);
CAMLprim value caml_gr_draw_arc(value *argv, int argc);
CAMLprim value caml_gr_draw_char(value chr);
CAMLprim value caml_gr_draw_image(value i, value x, value y);
CAMLprim value caml_gr_draw_rect(value vx, value vy, value vw, value vh);
CAMLprim value caml_gr_draw_string(value str);
CAMLprim value caml_gr_draw_text(value text,value x);
CAMLprim value caml_gr_dump_image (value img);
CAMLprim value caml_gr_fill_arc_nat(value vx, value vy, value vrx, value vry,
                                    value vstart, value vend);
CAMLprim value caml_gr_fill_arc(value *argv, int argc);
CAMLprim value caml_gr_fill_poly(value vect);
CAMLprim value caml_gr_fill_rect(value vx, value vy, value vw, value vh);
CAMLprim value caml_gr_get_mousex(value unit);
CAMLprim value caml_gr_get_mousey(value unit);
CAMLprim value caml_gr_lineto(value vx, value vy);
CAMLprim value caml_gr_make_image(value matrix);
CAMLprim value caml_gr_moveto(value vx, value vy);
CAMLprim value caml_gr_open_graph(value arg);
CAMLprim value caml_gr_plot(value vx, value vy);
CAMLprim value caml_gr_point_color(value vx, value vy);
CAMLprim value caml_gr_remember_mode(value flag);
CAMLprim value caml_gr_resize_window (value vx, value vy);
CAMLprim value caml_gr_set_color(value vcolor);
CAMLprim value caml_gr_set_font(value fontname);
CAMLprim value caml_gr_set_line_width(value vwidth);
CAMLprim value caml_gr_set_text_size (value sz);
CAMLprim value caml_gr_set_window_title(value text);
CAMLprim value caml_gr_show_bitmap(value filename,int x,int y);
CAMLprim value caml_gr_sigio_handler(value unit);
CAMLprim value caml_gr_sigio_signal(value unit);
CAMLprim value caml_gr_size_x(value unit);
CAMLprim value caml_gr_size_y(value unit);
CAMLprim value caml_gr_sound(value freq, value vdur);
CAMLprim value caml_gr_synchronize(value unit);
CAMLprim value caml_gr_text_size(value str);
CAMLprim value caml_gr_wait_event(value eventlist);
CAMLprim value caml_mutex_lock(value wrapper);
CAMLprim value caml_mutex_new(value unit);
CAMLprim value caml_mutex_try_lock(value wrapper);
CAMLprim value caml_mutex_unlock(value wrapper);
CAMLprim value caml_thread_cleanup(value unit);
CAMLprim value caml_thread_exit(value unit);
CAMLprim value caml_thread_id(value th);
CAMLprim value caml_thread_initialize(value unit);
CAMLprim value caml_thread_join(value th);
CAMLprim value caml_thread_new(value clos);
CAMLprim value caml_thread_self(value unit);
CAMLprim value caml_thread_sigmask(value cmd, value sigs);
CAMLprim value caml_thread_uncaught_exception(value exn);
CAMLprim value caml_thread_yield(value unit);
CAMLprim value caml_wait_signal(value sigs);
CAMLprim value camltk_add_file_input(value fd, value cbid);
CAMLprim value camltk_add_file_output(value fd, value cbid);
CAMLprim value camltk_add_timer(value milli, value cbid);
CAMLprim value camltk_dooneevent(value flags);
CAMLprim value camltk_finalize(value unit);
CAMLprim value camltk_getimgdata (value imgname);
CAMLprim value camltk_getvar(value var);
CAMLprim value camltk_init(value v);
CAMLprim value camltk_opentk(value argv);
CAMLprim value camltk_rem_file_input(value fd, value cbid);
CAMLprim value camltk_rem_file_output(value fd, value cbid);
CAMLprim value camltk_rem_timer(value token);
CAMLprim value camltk_return (value v);
CAMLprim value camltk_setvar(value var, value contents);
CAMLprim value camltk_splitlist (value v);
CAMLprim value camltk_tcl_direct_eval(value v);
CAMLprim value camltk_tcl_eval(value str);
CAMLprim value camltk_tk_mainloop(value unit);
CAMLprim value camltk_trace_var(value var, value cbid);
CAMLprim value camltk_untrace_var(value var, value cbid);
CAMLprim value camltk_wait_des(value win, value cbid);
CAMLprim value camltk_wait_vis(value win, value cbid);
CAMLprim value compare_digits_nat(value nat1, value ofs1,
                                  value nat2, value ofs2);
CAMLprim value compare_nat(value *argv, int argn);
CAMLprim value complement_nat(value nat, value ofs, value len);
CAMLprim value create_nat(value size);
CAMLprim value decr_nat(value nat, value ofs, value len, value carry_in);
CAMLprim value div_digit_nat(value *argv, int argn);
CAMLprim value div_nat(value *argv, int argn);
CAMLprim value incr_nat(value nat, value ofs, value len, value carry_in);
CAMLprim value initialize_nat(value unit);
CAMLprim value is_digit_int(value nat, value ofs);
CAMLprim value is_digit_normalized(value nat, value ofs);
CAMLprim value is_digit_odd(value nat, value ofs);
CAMLprim value is_digit_zero(value nat, value ofs);
CAMLprim value land_digit_nat(value nat1, value ofs1, value nat2, value ofs2);
CAMLprim value length_nat(value nat);
CAMLprim value lor_digit_nat(value nat1, value ofs1, value nat2, value ofs2);
CAMLprim value lxor_digit_nat(value nat1, value ofs1, value nat2, value ofs2);
CAMLprim value mult_digit_nat(value *argv, int argn);
CAMLprim value mult_nat(value *argv, int argn);
CAMLprim value nth_digit_nat_native(value nat, value ofs);
CAMLprim value nth_digit_nat(value nat, value ofs);
CAMLprim value num_digits_nat(value nat, value ofs, value len);
CAMLprim value num_leading_zero_bits_in_digit(value nat, value ofs);
CAMLprim value re_partial_match(value re, value str, value pos);
CAMLprim value re_replacement_text(value repl, value groups, value orig);
CAMLprim value re_search_backward(value re, value str, value startpos);
CAMLprim value re_search_forward(value re, value str, value startpos);
CAMLprim value re_string_match(value re, value str, value pos);
CAMLprim value set_digit_nat_native(value nat, value ofs, value digit);
CAMLprim value set_digit_nat(value nat, value ofs, value digit);
CAMLprim value set_to_zero_nat(value nat, value ofs, value len);
CAMLprim value shift_left_nat(value *argv, int argn);
CAMLprim value shift_right_nat(value *argv, int argn);
CAMLprim value square_nat(value *argv, int argn);
CAMLprim value sub_nat(value *argv, int argn);
CAMLprim value unix_accept(value sock);
CAMLprim value unix_access(value path, value perms);
CAMLprim value unix_alarm(value t);
CAMLprim value unix_bind(value socket, value address);
CAMLprim value unix_chdir(value path);
CAMLprim value unix_chmod(value path, value perm);
CAMLprim value unix_chown(value path, value uid, value gid);
CAMLprim value unix_chroot(value path);
CAMLprim value unix_clear_close_on_exec(value fd);
CAMLprim value unix_clear_nonblock(value fd);
CAMLprim value unix_closedir(value vd);
CAMLprim value unix_close(value fd);
CAMLprim value unix_connect(value socket, value address);
CAMLprim value unix_dup2(value fd1, value fd2);
CAMLprim value unix_dup(value fd);
CAMLprim value unix_environment(value unit);
CAMLprim value unix_error_message(value err);
CAMLprim value unix_execve(value path, value args, value env);
CAMLprim value unix_execvpe(value path, value args, value env);
CAMLprim value unix_execvp(value path, value args);
CAMLprim value unix_execv(value path, value args);
CAMLprim value unix_exit(value n);
CAMLprim value unix_fchmod(value fd, value perm);
CAMLprim value unix_fchown(value fd, value uid, value gid);
CAMLprim value unix_fork(value unit);
CAMLprim value unix_fstat_64(value fd);
CAMLprim value unix_fstat_64(value handle);
CAMLprim value unix_fstat(value fd);
CAMLprim value unix_fstat(value handle);
CAMLprim value unix_ftruncate_64(value fd, value len);
CAMLprim value unix_ftruncate(value fd, value len);
CAMLprim value unix_getaddrinfo(value vnode, value vserv, value vopts);
CAMLprim value unix_getcwd(value unit);
CAMLprim value unix_getegid(value unit);
CAMLprim value unix_geteuid(value unit);
CAMLprim value unix_getgid(value unit);
CAMLprim value unix_getgrgid(value gid);
CAMLprim value unix_getgrnam(value name);
CAMLprim value unix_getgroups(value unit);
CAMLprim value unix_gethostbyaddr(value a);
CAMLprim value unix_gethostbyaddr(value name);
CAMLprim value unix_gethostbyname(value name);
CAMLprim value unix_gethostname(value unit);
CAMLprim value unix_getitimer(value which);
CAMLprim value unix_getlogin(value unit);
CAMLprim value unix_getnameinfo(value vaddr, value vopts);
CAMLprim value unix_getpeername(value sock);
CAMLprim value unix_getpid(value unit);
CAMLprim value unix_getppid(value unit);
CAMLprim value unix_getprotobyname(value name);
CAMLprim value unix_getprotobynumber(value proto);
CAMLprim value unix_getpwnam(value name);
CAMLprim value unix_getpwuid(value uid);
CAMLprim value unix_getservbyname(value name, value proto);
CAMLprim value unix_getservbyport(value port, value proto);
CAMLprim value unix_getsockname(value sock);
CAMLprim value unix_getsockopt(value vty, value vsocket, value voption);
CAMLprim value unix_gettimeofday(value unit);
CAMLprim value unix_getuid(value unit);
CAMLprim value unix_gmtime(value t);
CAMLprim value unix_inet_addr_of_string(value s);
CAMLprim value unix_initgroups(value user, value group);
CAMLprim value unix_isatty(value fd);
CAMLprim value unix_kill(value pid, value signal);
CAMLprim value unix_link(value path1, value path2);
CAMLprim value unix_listen(value sock, value backlog);
CAMLprim value unix_localtime(value t);
CAMLprim value unix_lockf(value fd, value cmd, value span);
CAMLprim value unix_lseek_64(value fd, value ofs, value cmd);
CAMLprim value unix_lseek(value fd, value ofs, value cmd);
CAMLprim value unix_lstat_64(value path);
CAMLprim value unix_lstat(value path);
CAMLprim value unix_mkdir(value path, value perm);
CAMLprim value unix_mkfifo(value path, value mode);
CAMLprim value unix_mktime(value t);
CAMLprim value unix_nice(value incr);
CAMLprim value unix_opendir(value path);
CAMLprim value unix_open(value path, value flags, value perm);
CAMLprim value unix_pipe(value unit);
CAMLprim value unix_putenv(value name, value val);
CAMLprim value unix_readdir(value vd);
CAMLprim value unix_readlink(value path);
CAMLprim value unix_read(value fd, value buf, value ofs, value vlen);
CAMLprim value unix_recvfrom(value sock, value buff, value ofs, value len,
                             value flags);
CAMLprim value unix_recv(value sock, value buff, value ofs, value len,
                         value flags);
CAMLprim value unix_rename(value path1, value path2);
CAMLprim value unix_rewinddir(value d);
CAMLprim value unix_rewinddir(value vd);
CAMLprim value unix_rmdir(value path);
CAMLprim value unix_select(value readfds, value writefds, value exceptfds,
                           value timeout);
CAMLprim value unix_sendto(value *argv, int argc);
CAMLprim value unix_sendto_native(value sock, value buff, value ofs, value len,
                                  value flags, value dest);
CAMLprim value unix_sendto(value *argv, int argc);
CAMLprim value unix_send(value sock, value buff, value ofs, value len,
                         value flags);
CAMLprim value unix_set_close_on_exec(value fd);
CAMLprim value unix_setgid(value gid);
CAMLprim value unix_setgroups(value groups);
CAMLprim value unix_setitimer(value which, value newval);
CAMLprim value unix_set_nonblock(value fd);
CAMLprim value unix_setsid(value unit);
CAMLprim value unix_setsockopt(value vty, value socket, value option,
                               value val);
CAMLprim value unix_setuid(value uid);
CAMLprim value unix_shutdown(value sock, value cmd);
CAMLprim value unix_sigpending(value unit);
CAMLprim value unix_sigprocmask(value vaction, value vset);
CAMLprim value unix_sigsuspend(value vset);
CAMLprim value unix_single_write(value fd, value buf, value vofs, value vlen);
CAMLprim value unix_sleep(value t);
CAMLprim value unix_socket(value domain, value type, value proto);
CAMLprim value unix_socketpair(value domain, value type, value proto);
CAMLprim value unix_socket(value domain, value type, value proto);
CAMLprim value unix_stat_64(value path);
CAMLprim value unix_stat(value path);
CAMLprim value unix_string_of_inet_addr(value a);
CAMLprim value unix_symlink(value path1, value path2);
CAMLprim value unix_tcdrain(value fd);
CAMLprim value unix_tcflow(value fd, value action);
CAMLprim value unix_tcflush(value fd, value queue);
CAMLprim value unix_tcgetattr(value fd);
CAMLprim value unix_tcsendbreak(value fd, value delay);
CAMLprim value unix_tcsetattr(value fd, value when, value arg);
CAMLprim value unix_times(value unit);
CAMLprim value unix_time(value unit);
CAMLprim value unix_truncate_64(value path, value len);
CAMLprim value unix_truncate(value path, value len);
CAMLprim value unix_umask(value perm);
CAMLprim value unix_unlink(value path);
CAMLprim value unix_utimes(value path, value atime, value mtime);
CAMLprim value unix_waitpid(value flags, value pid_req);
CAMLprim value unix_wait(value unit);
CAMLprim value unix_write(value fd, value buf, value vofs, value vlen);
CAMLprim value win_cleanup(value unit);
CAMLprim value win_clear_close_on_exec(value fd);
CAMLprim value win_create_process(value *argv, int argn);
CAMLprim value win_filedescr_of_channel(value vchan);
CAMLprim value win_findclose(value valh);
CAMLprim value win_findfirst(value name);
CAMLprim value win_findnext(value valh);
CAMLprim value win_handle_fd(value vfd);
CAMLprim value win_inchannel_of_filedescr(value handle);
CAMLprim value win_outchannel_of_filedescr(value handle);
CAMLprim value win_set_close_on_exec(value fd);
CAMLprim value win_startup(value unit);
CAMLprim value win_system(value cmd);
CAMLprim value win_waitpid(value vflags, value vpid_req);
