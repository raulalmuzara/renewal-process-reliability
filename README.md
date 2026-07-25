# Analysis and simulation of a renewal process in reliability theory

An electronic system requires $m$ components to operate. These components are identical and operate independently. The lifespan of each component follows an exponential distribution of mean $1/\lambda$. The system is operational only if there are at least $m$ components operating simultaneously. When the system is not operational, the components that have not failed continue to function. We increase the system availability by placing redundant units and by periodic maintenance. We place $r$ redundant components in addition to the $m$ needed. Through periodic maintenance, the system is inspected every $T$ units of time. At each inspection time, non-operational components are replaced with new ones. The time it takes a technician to replace a component is negligible. Defective units can only be detected in these inspection times. For each working component, there is an operating cost $I$ per unit of time. There is also a fixed cost $K$ for each inspection and a repair cost $R$ for each component that must be replaced.

We will find:

1. The probability that the system will fail between two consecutive inspection periods.

2. The number $r$ of redundant units that would guarantee a probability that the system is operational $p_0$.

3. The asymptotic cost per unit of time of the system.

4. The asymptotic proportion of time that the system is operational.

For each question, a simulation is presented and we compare the result with the theoretical calculation in the attached PDF document. In all cases, we observe a clear agreement between experimental and theoretical results. All cells can be run to check the results or modify the value of the parameters. Seeds have been set to ensure reproducibility.

*renewal-process-analysis.pdf*: Document with mathematical explanations

*renewal-process-simulation.Rmd*: R Markdown notebook with cells for running code

*renewal-process-simulation.md*: Markdown export of the notebook to view the content directly on GitHub

*renewal-process-simulation.html*: HTML export of the notebook to view the content in a browser
