# Generating Strings from PEGs



This document describes the String generation process from a well formed PEG.

The main utility for such a process is  to be able to use test-base property on PEGs to

demonstrate the existence of errors  on desired properties of PEGs. 



There where 3 attempts to develop a String generator for PEGs, one which attempt

to develop a combinator-based generator, one based on the derivate of a PEG and finally

an alternative semantic for the Ierusalimschy's Virtual Machine. This last alternative

will be described in this document. 



## The Problem 



The problem seems innocent enough : Given a PEG  $G$ , generate a String (preferable at random) that 

will be accepted by $G$. 



At a first glance this problem seems quite trivial. Consider the PEG $$G_{A}$$ given below: 
$$
\begin{array}{l}
    A \leftarrow aA / . \\
    A
\end{array}
$$

Generate a string for the $$G_{A}$$ is trivial enough that even the methods for the GLC can be used here. 

We start with the nonterminal $$A$$ and follow the replacement process, i.e replacing the only occurrence of $$A$$ by the its body. For this grammar this will work :



|   PEG   | Action |
| --- | ---            |
| A   |               |
| aA| Expand left alternative A   |
| aaA | Expand left alternative A |
| aaaA | Expand left alternative A |
| aaa. | Expand Right alternative A |



This process dos not always work and can produce incorrect results. Consider the grammar $$G_B$$ 
$$
\begin{array}{l}    
    A \leftarrow aAa / . \\
    A
\end{array}
$$






This problem might, at first, look similar to the generation of a sentential form a context free grammar, 

however we can easily distinguish this problem from that one. 

  

  

Suppose that you are given the following PEG, supposed 
$$
\langle \{a,b\}, \{A\}, R, !A (a/b)^\star \rangle \\
\text{where } R \text{ is given by the set of rules:}\\
\begin{array}{l}
A \leftarrow aAb / \varepsilon \\
\end{array}
$$


## The Ierusalimschy's Virtual Machine:

<!--(Skip this section if you are already familiar with it)-->

This text provides a description of the PEG Virtual Machine , it's state and instructions and the brief 

description of the semantics of the instructions.  The optimizations are not discussed. 

The PEG Virtual Machine is composed of a State the sequence of instructions and  the input

String.  The machine state has the following  components: 


$$
\langle p ,i, stk, cap \rangle
$$
Where

- $p$ : Is a pointer to the next instruction to be executed. This field cal also assume the value **Fail** to denote a failed state.  
- $i$ : Is a pointer to the current character (also called subject) on the String.
- $$stk$$ : Is a stack that can contain either a single return address from a nonterminal call or a full execution context (code address, position on the String and a capture environment) representing a backtracking point. 
- $$cap$$ : Contains references to the start and end points of a partial matching of interest (captures). 



In order to keep the notation short we will 

The instructions of the PEG Virtual Machine are: 


- $$\langle p ,i, stk, cap \rangle \;  \textbf{Char c} \to \langle p+1 ,i+1, stk, cap \rangle$$ whenever Str[i]  : 





## Flipping the Machine Semantics





