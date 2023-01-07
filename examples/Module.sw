[defun add [(a Integer) (b Integer)] : Integer [+ a b]]

[defmodule Abc
  [defun add [(a Integer) (b Integer)] : Integer [+ a b]]
  [defun add2 [(c Integer)] : Integer [Abc:add 34 c]]
  [defmodule Xyz
    [defun add2 [(c Integer)] : Integer [Abc:add 2 c]]]]

// [defmodule Jkl [defmodule Iop ]]

[print !Integer [add 1 2]]