/*
 * Graph: generic graph library
 * Copyright (C) 2004
 * Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 */

/* $Id:$ */

%{
  open Dot_ast
  open Parsing
%} 

%token <Dot_ast.id> ID
%token COLON COMMA EQUAL SEMICOLON
%token STRICT GRAPH DIGRAPH LBRA RBRA LSQ RSQ NODE EDGE SUBGRAPH EOF

%type <Dot_ast.file> file
%start file
%%

file: 
| strict_opt graph_or_digraph id_opt LBRA stmt_list RBRA EOF
    { { strict = $1; digraph = $2; id = $3; stmts = $5 } }
;

strict_opt:
| /* epsilon */ { false }
| STRICT        { true }
;

graph_or_digraph:
| GRAPH   { false }
| DIGRAPH { true }
;

stmt_list:
| /* epsilon */ { [] }
| list1_stmt    { $1 }
;

list1_stmt:
| stmt semicolon_opt { [$1] }
| stmt semicolon_opt list1_stmt { $1 :: $3 }
;

semicolon_opt:
| /* epsilon */ { () }
| SEMICOLON     { () }
;

stmt:
| node_stmt { $1 }
;

node_stmt:
| node_id attr_list { Node_stmt ($1, $2) }
;

node_id:
| ID port_opt { $1, $2 }
;

port_opt:
| /* epsilon */ { None }
| port          { Some $1 }
;

port:
| COLON ID { PortId ($2, None) }
| COLON ID COLON compass_pt { PortId ($2, Some $4) }
| COLON compass_pt { PortC $2 }
;

compass_pt:
| ID 
    { match $1 with
      | Ident "n" -> N
      | Ident "ne" -> Ne
      | Ident "e" -> E
      | Ident "se" -> Se
      | Ident "s" -> S
      | Ident "sw" -> Sw
      | Ident "w" -> W
      | Ident "nw" -> Nw
      | _ -> raise Parse_error }
;

attr_list:
| /* epsilon */ { [] }
| list1_attr    { $1 }
;

list1_attr:
| LSQ a_list RSQ { [$2] }
| LSQ a_list RSQ list1_attr { $2 :: $4 }
;

id_opt:
| /* epsilon */ { None }
| ID            { Some $1 }
;

a_list:
| equality comma_opt { [$1] }
| equality comma_opt a_list { $1 :: $3 }
;

equality:
| ID { $1, None }
| ID EQUAL ID { $1, Some $3 }
;

comma_opt:
| /* epsilon */ { () }
| COMMA         { () }
;
