%%%-------------------------------------------------------------------
%%% File:      erlydtl_tsd_parser.yrl
%%% @author    Andreas Stenius <kaos@astekk.se>
%%% @copyright 2013 Andreas Stenius
%%% @doc
%%% Template Scanner Definition parser
%%% @end
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2013 Andreas Stenius
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2013-11-05 by Andreas Stenius
%%%-------------------------------------------------------------------

Nonterminals
  action actions arg args attr guard prefix rule rule_body prio
  scanner scanner_exp state state_in state_new state_next states tag
  tag_body tag_head tag_op.
  
Terminals
  '+' '-' ':' ',' '.'
  any code identifier number skip string until.

Rootsymbol
  scanner.

Expect 1.

%% ----------------------------------------
%% ----------------------------------------

scanner -> scanner_exp : ['$1'].
scanner -> scanner_exp scanner : ['$1' | '$2'].

%% A Scanner is made up of `attr' `rule', `tag' and `form' expressions.
scanner_exp -> attr : {attr, '$1'}.
scanner_exp -> rule : {rule, '$1'}.
scanner_exp -> tag : {tag, '$1'}.
scanner_exp -> code '.' : {form, value_of('$1')}.

%% ----------------------------------------
%% `attr'
%% ----------------------------------------
attr -> '-' identifier args '.' : {value_of('$2'), '$3'}.


%% ----------------------------------------
%% `rule'
%% ----------------------------------------
rule -> prio prefix state_in guard ':' rule_body '.' : {rule, '$1', '$2', '$3', '$4', '$6'}.

prio -> number : {prio, value_of('$1')}.

prefix -> string : {prefix, value_of('$1')}.
prefix -> any : any_prefix.
prefix -> ':' : end_of_input.

state_in -> state : '$1'.
state_in -> any : any_state.
state_in -> any '+' : close_any_state.
state_in -> any '-' : any_stateless.

rule_body -> actions : {'$1', keep_state}.
rule_body -> state_new : {[], '$1'}.
rule_body -> actions ',' state_next : {'$1', '$3'}.
rule_body -> ':' code : {code, value_of('$2')}.

actions -> action actions : ['$1' | '$2'].
actions -> skip : [].
actions -> '$empty' : [].

action -> identifier : {add, value_of('$1')}.
action -> identifier '-' string : {add, value_of('$1'), value_of('$3')}.
action -> '+' identifier : {append, value_of('$2')}.
action -> '+' identifier '-' string : {append, value_of('$2'), value_of('$4')}.
action -> string : {add, value_of('$1')}.

state_next -> state : '$1'.
state_next -> state_new : '$1'.

state_new -> identifier until string : {state, {value_of('$1'), value_of('$3')}}.


%% ----------------------------------------
%% `tag'
%% ----------------------------------------
tag -> tag_head guard ':' tag_body '.' : {tag, '$1', '$2', '$4'}.

tag_head -> states : lists:reverse('$1').
tag_head -> states ',' state : {lists:reverse('$1'), '$3'}.

tag_body -> tag_op : ['$1'].
tag_body -> tag_op ',' tag_body : ['$1' | '$3'].

tag_op -> args : '$1'.
tag_op -> ':' code : {code, value_of('$2')}.

states -> state : ['$1'].
states -> state states : ['$1' | '$2'].


%% ----------------------------------------
%% Common rules
%% ----------------------------------------

args -> arg : ['$1'].
args -> arg args : ['$1' | '$2'].

arg -> identifier : value_of('$1').
arg -> string : value_of('$1').

guard -> ',' code : {guard, value_of('$2')}.
guard -> '$empty' : {guard, []}.

state -> identifier : {state, value_of('$1')}.
state -> identifier '-' : {stateless, value_of('$1')}.


%% ----------------------------------------
Erlang code.
%% ----------------------------------------

value_of({_, _, Value}) -> Value.
