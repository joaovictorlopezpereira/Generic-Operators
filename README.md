# Generic Operators

## Description

This is an implementation of an arithmetic system inspired by Harold Abelson and Gerald Jay Sussman's system in Structure and Interpretation of Computer Programs (SICP).

To avoid creating a large, static function for selecting the appropriate procedure to operate on two given types, a more effective approach is to use Data-Directed Programming. In this approach, procedures are organized in a tree-like structure, which is traversed to locate the correct function for the relevant data. Each data element is tagged with its type, a technique known as tagged data, enabling the system to efficiently dispatch the operation to the appropriate procedure. Additionally, coercion is employed to facilitate operations between different types, leveraging their hierarchical organization in a tower of types.

A better explanation about the techniques used can be found at the ``brief-context.pdf`` file.

-----

## About the Language Used

The code is implemented in ``MIT Scheme``, the programming language used in SICP. We chose ``MIT Scheme`` instead of ``Scheme`` because we update the table where procedures are stored using ``set-cdr!``, a procedure no longer available in modern ``Scheme`` versions. Although the usage of this procedure is not strictly necessary, the functions can be updated to run in modern ``Scheme`` with minor modifications.

-----

## About the Slides

This study, presented by João Victor Lopez Pereira at the 13th _Semana de Integração Acadêmica_ (SIAc) event at the Federal University of Rio de Janeiro (UFRJ), is accompanied by the slides in the file ``slides.pdf``.

In addition to the arithmetic system, we compare the use of pairs in ``Lisp`` with hash tables in ``Lua``, examining their complexity, simplicity, and the design goals of each language. This analysis highlights the different approaches and advantages in their implementation and usage.

-----

## Bibliography:

  - ABELSON, Harold; SUSSMAN, Gerald Jay. Structure and Interpretation of 
    Computer Programs. 2nd ed. Cambridge, MA: The MIT Press, 1984. 
    ISBN 0-262-01153-0.

  - ABELSON, Harold. Lecture 4B: Generic Operators. MIT 6.001 Structure and 
    Interpretation of Computer Programs, Spring 1986. Available at: 
    https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/. 
    Accessed August 8, 2024.

Any additional bibliography used is cited in the ``brief-context.pdf`` and ``slides.pdf`` files.
