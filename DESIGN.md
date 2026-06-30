# Design Decisions

## DD-001

O GA é a metaheurística principal.

## DD-002

PSO atua somente sobre a elite (10%).

## DD-003

GSO não movimenta indivíduos.

Utiliza apenas o mecanismo de vizinhança.

## DD-004

A diversidade será estimada pela densidade populacional média

ρ
ˉ
	​
## DD-005

A taxa de mutação será adaptativa

p
m
	​

=p
m,min
	​

+
ρ
ˉ
(p
m,max
−p
m,min
)

## DD-006

Tratamento das restrições:

Penalidade dinâmica linear

F(x)=f(x)+λ(t)V(x)

## DD-007

Comparação experimental

GA
PSO
GSO
AGD-PSO

## DD-008

Benchmarks

G01
G06
G10

## DD-009

31 execuções independentes.

## DD-010

Mesmo orçamento computacional (NFE).

## DD-011

Teste estatístico

Kruskal–Wallis.

## DD-012

CSV por geração + CSV resumo por execução.