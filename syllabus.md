# CC-SICP-CFD: The "Crazy Combination" Syllabus

**Repo:** `cc-sicp-cfd`
**Goal:** Master the intersection of **Structure and Interpretation of Computer Programs (SICP)** and **Computational Fluid Dynamics (CFD)** in 9 months.
**Schedule:** A/B Split (2 Hours Daily)

## The Protocol

The "A/B Split" is designed to manage cognitive load. Do not mix contexts on the same day.

* **Day A (2 Hours):** **SICP (Scheme/Racket)**. Focus on abstraction, recursion, and metalinguistic theory.
* **Day B (2 Hours):** **CFD (C++ / CUDA)**. Focus on numerics, memory management, and physical implementation.
* **Day 7:** Rest or Catch-up.

---

## Phase 1: The Foundation (Months 1 – 3)
**Theme:** *Abstraction & The Grid*
**Goal:** Build a robust 1D Solver where the "Physics" is separated from the "Math."

| Week | Track | Topic / Reading | The "Project Task" (Code It) |
| :--- | :--- | :--- | :--- |
| **1-2** | **SICP** | **1.1: The Elements of Programming.** (Naming, Environment, Evaluation). | **Newton's Root Finder:** Write a square root finder in Scheme using Newton's Method. *Goal: Understand iterative convergence.* |
| | **CFD** | **Ch 1: Basic Conservation Equations.** (Navier-Stokes overview). | **Field Initialization:** Write a C++ script that allocates a 1D array (std::vector) and initializes a temperature field $T(x) = \sin(x)$. |
| **3-4** | **SICP** | **1.3: Higher-Order Procedures.** (Functions taking functions). | **Generic Summation:** Write a generic `sum` function that takes a `term` function (e.g., $term(x) = x^2$) and a `next` function. |
| | **CFD** | **Ch 3: Finite Volume Method (Diffusion).** | **1D Diffusion Solver:** Write a C++ solver for $d^2T/dx^2 = 0$. Hardcode the grid. Use the **TDMA** (Tridiagonal Matrix Algorithm). |
| **5-8** | **SICP** | **2.1 - 2.2: Data Abstraction.** (Constructors/Selectors). | **Rational Numbers:** Implement a `Rational` number system. Hide the internal representation (pairs) behind strict barriers. |
| | **CFD** | **Ch 4: FVM (Convection-Diffusion).** | **Refactor to Classes:** Create a `Mesh` class and a `Cell` struct. The solver must loop over `mesh.cells()` instead of a raw integer `i`. |

### 🛑 Boss Fight 1: The Generic 1D Solver
**Objective:** Build a C++ solver for the 1D Convection-Diffusion equation.
**Requirement:** You must be able to swap the differencing scheme (Upwind vs. Central Difference) at runtime by passing a `Scheme` strategy object (inspired by SICP 1.3).

---

## Phase 2: The Core (Months 4 – 6)
**Theme:** *State, Time, and Multidimensionality*
**Goal:** Move to 2D and handle Time-Stepping without spaghetti code.

| Week | Track | Topic / Reading | The "Project Task" (Code It) |
| :--- | :--- | :--- | :--- |
| **9-11** | **SICP** | **2.4 - 2.5: Generic Operations.** (Tagging data, complex systems). | **Arithmetic Package:** Build a system that adds "Scheme Numbers" and "Rational Numbers" seamlessly using data tagging. |
| | **CFD** | **Ch 5: Solution of Linear Equation Systems.** | **Linear Solver Class:** Implement **Gauss-Seidel** and **SOR** solvers in C++. Abstract them behind a `LinearSolver` base class. |
| **12-14** | **SICP** | **3.1: Assignment & State.** (The costs of `set!`). | **Monte Carlo Simulation:** Implement the Monte Carlo $\pi$ estimator using `rand` and internal state. *Goal: Understand the dangers of mutation.* |
| | **CFD** | **Ch 6: Time Integration.** (Explicit vs Implicit). | **Unsteady Solver:** Implement an **Explicit Euler** loop (watch it become unstable). Then implement **Implicit Euler**. |
| **15-16** | **SICP** | **3.3: Mutable Data.** (Queues/Tables). | **Queue Implementation:** Build a Queue from scratch using mutable cons cells (or Atoms/Vectors if using Clojure). |
| | **CFD** | **Ch 7: Complex Geometries.** (Unstructured grids). | **2D Grid Support:** Update `Mesh` to support 2D. Use a flat 1D vector for storage ($idx = y \times width + x$) for cache efficiency. |

### 🛑 Boss Fight 2: The 2D Heat Equation
**Objective:** A C++ solver for the unsteady 2D Heat Equation on a rectangular plate.
**Requirement:** The code must use a `Field` class that abstracts the 2D indexing. The update loop should look like math: `T_new = T + dt * Laplacian(T)`.

---

## Phase 3: The Synthesis & Acceleration (Months 7 – 9)
**Theme:** *Concurrency, Metalinguistic Abstraction & The GPU*
**Goal:** The "Lid-Driven Cavity" running on CUDA.

| Week | Track | Topic / Reading | The "Project Task" (Code It) |
| :--- | :--- | :--- | :--- |
| **17-18** | **SICP** | **3.4: Concurrency.** (Serializer, Mutex). | **Bank Account Attack:** Implement the "Exchange" problem and create a deadlock. *Goal: Understand race conditions.* |
| | **CFD** | **CUDA Basics.** (Memory & Kernels). | **Field Transfer:** Write a wrapper to `malloc` memory on GPU and transfer your `Field` data to the device. Write a kernel to initialize $T=0$. |
| **19-20** | **SICP** | **3.5: Streams.** (Delayed evaluation). | **Infinite Primes:** Write a stream that generates infinite prime numbers. *Goal: Apply "Lazy" thinking to boundary condition updates.* |
| | **CFD** | **Ch 7-8: Navier-Stokes Equations.** | **SIMPLE Algorithm (CPU):** Implement the Semi-Implicit Method for Pressure Linked Equations on CPU first. Validate results. |
| **21-23** | **SICP** | **4.1: The Metacircular Evaluator.** | **The Interpreter:** Write `eval` and `apply`. *Goal: Deeply understand how an interpreter walks a syntax tree.* |
| | **CFD** | **CUDA Solvers.** (Jacobi Iteration). | **GPU Linear Solver:** Replace your CPU Gauss-Seidel solver with a **Jacobi Solver** running on GPU (Red-Black ordering is optimal here). |

### 🏆 Capstone Boss Fight: The CUDA Lid-Driven Cavity
**Objective:** Solve the incompressible flow in a box where the top lid moves at 1 m/s, accelerated by NVIDIA CUDA.
**Deliverables:**
1. **Hybrid Architecture:** A `Simulation` class that manages CPU logic (timestep loop) and GPU execution (flux calculation, linear solve).
2. **Performance Benchmark:** Graph the "Seconds per Timestep" of your CPU solver vs. your GPU solver for grid sizes $32^2, 64^2, 128^2, 256^2$.
3. **Visuals:** Export results to VTK and visualize the central vortex in **Paraview**.

---

## Resources
* **SICP:** [Video Lectures (Brian Harvey)](https://www.youtube.com/playlist?list=PLhMnuBfGeCDNgVzLPx9H472TueD8u9CNC)
* **CFD:** [Milovan Perić Video Series](https://www.youtube.com/watch?v=8a0j2DQiTVQ)
* **CUDA:** [NVIDIA CUDA C++ Programming Guide](https://docs.nvidia.com/cuda/cuda-c-programming-guide/)
* **Language:** DrRacket (SICP) & Modern C++ (CFD)