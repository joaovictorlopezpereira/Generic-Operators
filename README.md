This is my implementation of an arithmetic system inspired by Harold Abelson
and Gerald Jay Sussman's system in ``Structure and Interpretation of 
Computer Programs''.

To avoid creating a large function to call the appropriate procedure for 
operating on two given types, we built a ``tower of types.'' This allows us to 
easily use coercion to operate on our representations. By using this method,
we only need around n procedures, where n is the number of representations we
have. If we were to implement all conversion procedures individually, we would
need close to n^2 procedures.

We are using a technique called data-directed programming, in which we store
our procedures in a tree-like structure and traverse the tree to find the
correct function to operate on the relevant data. Each data type contains a
tag, which is used to find the appropriate procedure in the tree, a technique
known as tagged data.


Bibliography:

- ABELSON, Harold; SUSSMAN, Gerald Jay. Structure and Interpretation of Computer
Programs. 2nd ed. Cambridge, MA: The MIT Press, 1984. ISBN 0-262-01153-0.

- ABELSON, Harold. Lecture 4B: Generic Operators. MIT 6.001 Structure and
Interpretation of Computer Programs, Spring 1986. Available at: 
https://ocw.mit.edu/courses/6-001-structure-and-interpretation-of-computer-programs-spring-2005/. 
Accessed August 8, 2024.
