# gamebox-ecs

An implementation of the Entity-Component System (ECS) pattern, popular with
game development.

## Overview

This is an ECS implementation designed to be simple and flexible. It has not
been tested in the wild yet, so if you do so, please feel free to file issues or
lack of features you may encounter.

## Install

``` lisp
(ql:quickload :gamebox-ecs)
```

## Usage

First, initialize the ECS system:

``` lisp
(init-ecs)
```

You can optionally specify a logging message level of `:DEBUG` to see more
messages (the default is `:INFO`):

``` lisp
(init-ecs :log-level :debug)
```

Define some traits:

``` lisp
(deftrait test-trait-1 (x y z))
(deftrait test-trait-2 (x y z))
```

Define a behavior that is called for all GOBs that match.

`:MODE` is `R`, so it will be processed before any `R/W` behaviors, and `R/W`
will be processed before any `W` behaviors.

`:INTERVAL` is 2, so it will be called every two game ticks.

`:FILTERS` specifies the constraints for a GOB to be considered to have this
behavior.

`:GROUPING` a list of symbols specifying how many GOBs to include in parallel,
and unique names for them to be accessed. Every combination of this many GOBs
will be iterated over.

``` lisp
(defbehavior move ; the name of the behavior
    (:mode r ; R before R/W before W. This ensures proper order of mutable
             ; behaviors
     :interval 2 ; will be called only once every 2 game ticks
     :filters ((all groups test-group-1 test-group-2) ; must be a member of all
                                                      ; of these groups
               (any traits test-trait-1 test-trait-2) ; must have at least one
                                                      ; of these traits
               (none traits not-this-trait)) ; must not have this trait
     :grouping (e1 e2)) ; gobs will be combined in pairs so they can be compared
                        ; in parallel
  (format t "~A~%" (list e1 e2))) ; the body goes here - we just print a list of
                                  ; each grouping.
```

Define some GOBs (Game Objects):

``` lisp
(make-gob player (test-group-1 test-group-2)
  (test-trait-1 :x 1 :y 2 :z 3))
(make-gob enemy (test-group-1 test-group-2)
  (test-trait-2 :x 1 :y 2 :z 3))
(make-gob not-in-system (test-group-1)
  (test-trait-1 :x 1 :y 2 :z 3))
```

Cycle the behaviors. This should be called in your main loop:

``` lisp
(cycle-behaviors) ; The above behavior is called once every 2 ticks, so this
                  ; will have no effect.
(cycle-behaviors) ; This will affect some GOBs, since it is the second tick. It
                  ; will print: (2 1)
```

## License

Copyright © 2016 Michael Fiano <mail@michaelfiano.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
