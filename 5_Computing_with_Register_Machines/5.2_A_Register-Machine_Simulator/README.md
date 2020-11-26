# 5.2 A Register-Machine Simulator

### Exercise 5.7:

Use the simulator to test the machines you designed in Exercise 5.4.

## 5.2.1 The Machine Model

## 5.2.2 The Assembler

### Exercise 5.8:

The following register-machine code is ambiguous, because the label here is defined more than once:

```
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there
```

With the simulator as written, what will the contents of register `a` be when control reaches `there`? Modify the `extract-labels` procedure so that the assembler will signal an error if the same label name is used to indicate two different locations.

## 5.2.3 Generating Execution Procedures for Instructions

### Exercise 5.9:

The treatment of machine operations above permits them to operate on labels as well as on constants and the contents of registers. Modify the expression-processing procedures to enforce the condition that operations can be used only with registers and constants.

### Exercise 5.10:

Design a new syntax for register-machine instructions and modify the simulator to use your new syntax. Can you implement your new syntax without changing any part of the simulator except the syntax procedures in this section?
