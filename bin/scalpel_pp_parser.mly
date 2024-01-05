%{
  open Scalpel_program
%}

%token <string> IDENTIFER
%token <string> LITERAL_VALUE

// top-level declarations
%token TYPE_PRELUDE
%token MUTABLE_PRELUDE
%token FROZEN_PRELUDE
%token STATIC_PRELUDE
%token TYPE_ANNOTATION_PRELUDE

// instruction
%token TRUE_BRANCH_PRELUDE
%token FALSE_BRANCH_PRELUDE
%token LOOP_PRELUDE


// operators
%token ASSIGN

// symbol
%token <int>NON_NEGATIVE_INTEGER



// file structure control
%token COMMA
%token DOT

%token TYPE_ARGUMENT_OPENOR
%token TYPE_ARGUMENT_CLOSER

%token VALUE_ARGUMENT_OPENOR
%token VALUE_ARGUMENT_CLOSER

%token INSTRUCTIONS_OPENOR
%token INSTRUCTIONS_CLOSER


%token EOF

%start program
%type <Scalpel_program.definition> program

%%

program:
| list = nonempty_list(definition) EOF
    {
        list
    }
| error { failwith "error" }
;

definition:
| t = type_definition { Type t }
| f = function_definition { Function f }
;


type_definition:
| TYPE_PRELUDE id=IDENTIFER ASSIGN e=type_expression 
    {
        Scalpel_type.{identifier=id; expression=e}
    }
;


type_identifier:
    id = IDENTIFER {Scalpel_type.Identifier id}
;


type_symbol: 
    n = NON_NEGATIVE_INTEGER
    { Scalpel_type.Symbol n }
;


type_specialisation:
    id = IDENTIFER
    args = type_expression_list
    {
        Scalpel_type.Specialization(id, args)
    }
;


type_expression:
|    s = type_symbol { s }
|   id = type_identifier    { id }
|    e = type_specialisation { e }
;


type_expression_list:
    TYPE_ARGUMENT_OPENOR
        types=separated_list(COMMA, type_expression)
    TYPE_ARGUMENT_CLOSER
    { types }
;


instructions:
|   INSTRUCTIONS_OPENOR
        instructions = list(instruction)
    INSTRUCTIONS_CLOSER
    {instructions}


function_definition:
|   m=mutability
    name=IDENTIFER
    parameters=parameter_list
    TYPE_ANNOTATION_PRELUDE
    return=IDENTIFER
    instructions = instructions
    {
        Scalpel_function.{
            mutability = m;
            name;
            parameters;
            instructions;
            return
        }
    }
;

mutability:
|   MUTABLE_PRELUDE {Mutable}
|   FROZEN_PRELUDE {Frozen}
|   STATIC_PRELUDE {Static}
;

parameter_list:
| 
    VALUE_ARGUMENT_OPENOR
        parameters=separated_list(COMMA, parameter)
    VALUE_ARGUMENT_CLOSER
    {
        parameters
    }
;

parameter:
    name=IDENTIFER
    TYPE_ANNOTATION_PRELUDE
    typename=IDENTIFER 
    {
       Scalpel_value.{name; typename}
    }
;

value_expression:
| id = IDENTIFER {Variable id}
| value = LITERAL_VALUE {Literal value}
| c = construction {Scalpel_value.Construction c}
;

value_expression_chain:
| e = separated_nonempty_list(DOT, value_expression)
    {
        e
    }
;

value_expression_list:
    VALUE_ARGUMENT_OPENOR
    list = separated_list(COMMA, value_expression)
    VALUE_ARGUMENT_CLOSER
    { list }

construction:
    typename=IDENTIFER
    arguments=value_expression_list
    {
        {
            onHeap = false; 
            typename = typename; 
            arguments
        }
    }
;


symbol:
|   m = mutability id = IDENTIFER 
    {
       Scalpel_value.{
            mutability = m;
            name = id;
            typename = Inference
        }
    }
;


instruction:
|   s = symbol
    ASSIGN
    v = value_expression_chain { Initialization(s, v) }

|   id = IDENTIFER
    ASSIGN
    v = value_expression_chain { Assignment(id, v) }

|   TRUE_BRANCH_PRELUDE
    predicate = value_expression_chain
    true_branch = instructions
    FALSE_BRANCH_PRELUDE
    false_branch = instructions
    {
        Scalpel_instruction.Branching ({
            predicate;
            true_branch;
            false_branch
        })
    }

|   TRUE_BRANCH_PRELUDE
    predicate = value_expression_chain
    true_branch = instructions
    {
        Scalpel_instruction.Branching {
            predicate;
            true_branch;
            false_branch = []
        }
    }

|   LOOP_PRELUDE
    predicate = value_expression_chain
    body = instructions
    {
        Scalpel_instruction.Loop {
            predicate;
            body;
        }
    }
;

