# Programming-Language

A collection of F# assignments for the Programming Languages course.

## 📋 Overview

| Lab | Topic | Description |
|-----|-------|-------------|
| Lab1 | Warm-Up Exercise | F# basics practice (lists, recursion, trees, etc.) |
| Lab2 | CMinus | Simple imperative language interpreter implementation |
| Lab3 | FMinus | Simple functional language interpreter implementation |
| Lab4 | Type System | Type inference algorithm for F- language |

---

## 🔬 Lab1: Warm-Up Exercise

7 practice problems to learn F# fundamentals.

| Problem | Description | Key Concepts |
|---------|-------------|--------------|
| **P1** | `reverse` - Reverse a list | Recursion, Pattern matching, List concatenation (`@`) |
| **P2** | `eval` - Expression evaluator | Discriminated Union, Exception handling (`DivByZero`) |
| **P3** | `unzip` - Split list of pairs | `List.map`, `fst`, `snd` |
| **P4** | `sigma` - Summation function (Σ) | Higher-order functions, Recursion |
| **P5** | `digitsToInt` - Convert digit list to integer | `List.fold` |
| **P6** | `countMostFrequent` - Count mode frequency | `Map`, Reference variables (`ref`) |
| **P7** | `add` - Add node to binary search tree | Tree data structure, Pattern matching |

---

## 🖥️ Lab2: CMinus (Imperative Language)

Implement an interpreter for a simple imperative language **CMinus**.

### Supported Features
- **Expressions (Exp)**: Integer, Boolean, Variable, Arithmetic ops (`+`, `-`), Comparison ops (`<`, `>`, `==`, `!=`)
- **Statements (Stmt)**: NOP, Assignment, Sequential execution, Conditional (`if-else`), Loop (`while`)

### Core Functions
```fsharp
// Evaluate expression under given memory and return value
val evalExp : Exp -> Mem -> Val

// Execute statement and return updated memory
val exec : Stmt -> Mem -> Mem
```

---

## λ Lab3: FMinus (Functional Language)

Implement an interpreter for a simple functional language **FMinus**.

### Supported Features
- **Basic Expressions**: Integer, Boolean, Variable, Negation (`-E`)
- **Arithmetic/Comparison Ops**: `+`, `-`, `<`, `>`, `==`, `!=`
- **Conditional Expression**: `if E then E else E`
- **Variable Binding**: `let x = E in E`
- **Functions**: `let f x = E in E`, `let rec f x = E in E`, `fun x -> E`
- **Function Application**: `E E`

### Core Functions
```fsharp
// Evaluate expression under given environment and return value
val evalExp : Exp -> Env -> Val
```

### Closure Support
- `Func`: Regular function closure (parameter, body, environment)
- `RecFunc`: Recursive function closure (function name, parameter, body, environment)

---

## 🔍 Lab4: Type System (Type Inference)

Implement a type inference algorithm for the **FMinus** language.

### Type System
```fsharp
type Type =
  | Int                    // Integer type
  | Bool                   // Boolean type
  | TyVar of string        // Type variable
  | Func of Type * Type    // Function type (T -> T)
```

### Implemented Algorithms
1. **`gen`**: Type equation generation (Constraint Generation)
2. **`unify`**: Unification algorithm
3. **`solve`**: Solve type equations
4. **`infer`**: Infer program type

### Key Concepts
- **Occurs Check**: Prevent infinite types
- **Substitution**: Type variable replacement
- **Type Equation**: Collect and solve type constraints

---

## 🛠️ How to Run

```bash
# Run in each Lab directory
cd Lab1/P1
dotnet run

# Validate test cases
cd Lab2
python check.py
```

---

## 📁 Project Structure

```
Programming-Language/
├── Lab1/           # F# basic exercises
│   ├── P1~P7/      # Individual problem projects
│   └── check.py    # Auto-grading script
├── Lab2/           # CMinus interpreter
│   └── CMinus/
│       └── src/    # Lexer, Parser, AST, Interpreter
├── Lab3/           # FMinus interpreter  
│   └── FMinus/
│       └── src/    # AST, Interpreter
└── Lab4/           # Type inference system
    └── FMinusType/
        └── src/    # TypeSystem implementation
```