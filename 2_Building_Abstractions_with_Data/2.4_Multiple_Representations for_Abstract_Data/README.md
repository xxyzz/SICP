# 2.4 Multiple Representations for Abstract Data

## Exercise 2.73:

See page 248.

## Exercise 2.74:

See page 250.

## Exercise 2.75:

Implement the constructor `make-from-mag-ang` in message-passing style. This procedure should be analogous to the `make-from-real-imag` procedure given above.

## Exercise 2.76:

As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies—generic operations with explicit dispatch, data-directed style, and message-passing-style describe the changes that must be made to a system in order to add new types or new operations. Which organization would be most appropriate for a system in which new types must often be added? Which would be most appropriate for a system in which new operations must often be added?

explicit dispatch: add new type needs to modify all operations and have to make sure all produces name are distinct, add new operation needs to implement new operation produces for each type.

data-directed: add new type needs to put new operations of the type to the table, add new operation is same.

message-passing-style: add new type needs to add a new constructor, add new operation needs to modify all constructors.
