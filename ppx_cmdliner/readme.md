# ppx_cmdliner

This is a simple ppx designed to generate two functions of cmdliner. It generates the two functions passed to `Cmd.v`.

# Usage

Lets say you want to declare a custom command for command liner.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string;
        count: int;
    }
    [@@deriving cmdliner]

    let run who count =
        let aux count = if count = 0 then Ok () else print_endline "Hello" ^ who;
        aux (count - 1) in

        aux who count
end

let () =
  exit
  @@ Cmd.eval_result
  @@ Cmd.group (Cmd.info "example")
       [Cmd.v Greet_cmd.info (Greet_cmd.term Greet_cmd.run)]
```

The run function is your buisness function, you do not have to care about Cmdliner.
As you see, the run function takes in parameters each field of the record with the same type of fields.

## Some attributes

The ppx comes with some useful attributes:

### Custom command name 

By default the name of your command is "command".
You can use the `[@@cmdliner.name "the_command_name"]`.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string;
        count: int;
    }
    [@@cmdliner.name "greet"]
    [@@deriving cmdliner]

    ...
end
```

### Documentations

By default there isn't any documentations on your command.
You can add some documentations with:
 - `[@@cmdliner.doc "a short documentation"]` : should be a short documentation in one line.
 - `[@@cmdliner.desc "a big description"]` : is the description of your function, you will see this description in the "description" section of the manpage.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string;
        count: int;
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the given user in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

You can add documentation on parameters too.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string; [@cmdliner.doc "The person to greet"]
        count: int; [@cmdliner.doc "The number of times to greet the user"]
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the people given in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

### Argument assertions

Let's say you want to assert something on your parameter, but you don't want to declare a new type.
You can add an assertion function on parameters with `[@cmdliner.assert]`.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string; 
            [@cmdliner.doc "The person to greet"] 
            [@cmdliner.assert fun name -> String.length name <> 0]
        count: int; 
            [@cmdliner.doc "The number of times to greet the user"]
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the people given in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

In this way you specify that the `who` parameter can't be empty

### Default value

You can define a default value to your parameter, with `[@cmdliner.default your_value]`.
Be careful, when you use this attribute, your argument will be transform into a positional argument.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string; 
            [@cmdliner.doc "The person to greet"] 
            [@cmdliner.assert fun name -> String.length name <> 0]
        count: int; 
            [@cmdliner.doc "The number of times to greet the user"]
            [@cmdliner.default 3]
            
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the people given in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

### Env variable

You can define an env variable to replace your parameter, with `[@cmdliner.env]`.
Be careful, when you use this attribute, your argument will be transform into a positional argument.
The name of the env variable will be the same as your parameter name in uppercase.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string; 
            [@cmdliner.doc "The person to greet"] 
            [@cmdliner.assert fun name -> String.length name <> 0]
        count: int; 
            [@cmdliner.doc "The number of times to greet the user"]
            [@cmdliner.default 3]
            [@cmdliner.env]
            
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the people given in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

In this example you can use `--count` to set the parameter, or you can use `COUNT` env variable.

### Positional argument

You can force an argument to be a positionnal argument. It has to be an optional. Every optional argument are considered as positional argument.

```ocaml
module Greet_cmd = 
    type parameters = {
        who: string; 
            [@cmdliner.doc "The person to greet"] 
            [@cmdliner.assert fun name -> String.length name <> 0]
        count: int option; 
    }
    [@@cmdliner.name "greet"]
    [@@cmdliner.doc "greets people n times"]
    [@@cmdliner.desc "greets the people given in argument, n times, where n is given by the user"]
    [@@deriving cmdliner]
    ...
end
```

### Custom types

You can define a custom. To do so you have to define a module for your custom type which should respect the following signature:

```ocaml
module type CUSTOM_TYPE = sig
  type t

  val of_string : string -> t option

  val to_string : t -> string
end
```

```ocaml
module User = struct
    type t = 
        | World
        | User of string
    
    let of_string string = 
        match string with
            | "world" -> Some World
            | user -> if String.length user <> 0 then Some(User user) else None

    let to_string t = match t with
        | World -> "World"
        | User user -> user
end

module Greet_cmd = struct
    type parameters = {
        who: User.t; 
        count: int option; 
    }
    [@@deriving cmdliner]
    ...
end
```