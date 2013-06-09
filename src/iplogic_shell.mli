(* Copyright (C) 2012--2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** A specialized monoid for expressing a single shell command. *)
type shell_args =
   | AQ of string		(** Quoted command or argument. *)
   | AV of string		(** Verbatim command or argument. *)
   | AL of shell_args list	(** Command and argument list. *)

(** A specialized monoid for expressing a sequence of shell commands. *)
type shell_seq =
   | SC of shell_args		(** A single command. *)
   | SL of shell_seq list	(** A sequence of commands. *)

val shell_quoted : string -> string

val pop_first_shell_arg : shell_args -> (shell_args * shell_args) option

val output_shell_args : out_channel -> shell_args -> unit

val output_shell_seq : ?prefix: string -> out_channel -> shell_seq -> unit
