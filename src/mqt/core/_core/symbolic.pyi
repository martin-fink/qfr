from collections.abc import Iterator, Mapping, Sequence
from typing import overload

from .._compat.typing import Self

class Variable:
    def __eq__(self: Self, arg0: object) -> bool: ...
    def __gt__(self: Self, arg0: Variable) -> bool: ...
    def __hash__(self: Self) -> int: ...
    def __init__(self: Self, name: str = "") -> None: ...
    def __lt__(self: Self, arg0: Variable) -> bool: ...
    def __ne__(self: Self, arg0: object) -> bool: ...
    @property
    def name(self: Self) -> str: ...

class Term:
    def __eq__(self: Self, arg0: object) -> bool: ...
    def __hash__(self: Self) -> int: ...
    def __init__(self: Self, variable: Variable, coefficient: float = 1.0) -> None: ...
    def __mul__(self: Self, arg0: float) -> Term: ...
    def __ne__(self: Self, arg0: object) -> bool: ...
    def __rmul__(self: Self, arg0: float) -> Term: ...
    def __rtruediv__(self: Self, arg0: float) -> Term: ...
    def __truediv__(self: Self, arg0: float) -> Term: ...
    def add_coefficient(self: Self, coeff: float) -> None: ...
    def evaluate(self: Self, assignment: Mapping[Variable, float]) -> float: ...
    def has_zero_coefficient(self: Self) -> bool: ...
    @property
    def coefficient(self: Self) -> float: ...
    @property
    def variable(self: Self) -> Variable: ...

class Expression:
    constant: float
    @overload
    def __add__(self: Self, arg0: Expression) -> Expression: ...
    @overload
    def __add__(self: Self, arg0: Term) -> Expression: ...
    @overload
    def __add__(self: Self, arg0: float) -> Expression: ...
    def __eq__(self: Self, arg0: object) -> bool: ...
    def __getitem__(self: Self, idx: int) -> Term: ...
    def __hash__(self: Self) -> int: ...
    @overload
    def __init__(self: Self, terms: Sequence[Term], constant: float = 0.0) -> None: ...
    @overload
    def __init__(self: Self, term: Term, constant: float = 0.0) -> None: ...
    @overload
    def __init__(self: Self, constant: float = 0.0) -> None: ...
    def __iter__(self: Self) -> Iterator[Term]: ...
    def __len__(self: Self) -> int: ...
    def __mul__(self: Self, arg0: float) -> Expression: ...
    def __ne__(self: Self, arg0: object) -> bool: ...
    @overload
    def __radd__(self: Self, arg0: Term) -> Expression: ...
    @overload
    def __radd__(self: Self, arg0: float) -> Expression: ...
    def __rmul__(self: Self, arg0: float) -> Expression: ...
    @overload
    def __rsub__(self: Self, arg0: Term) -> Expression: ...
    @overload
    def __rsub__(self: Self, arg0: float) -> Expression: ...
    def __rtruediv__(self: Self, arg0: float) -> Expression: ...
    @overload
    def __sub__(self: Self, arg0: Expression) -> Expression: ...
    @overload
    def __sub__(self: Self, arg0: Term) -> Expression: ...
    @overload
    def __sub__(self: Self, arg0: float) -> Expression: ...
    def __truediv__(self: Self, arg0: float) -> Expression: ...
    def evaluate(self: Self, assignment: Mapping[Variable, float]) -> float: ...
    def is_constant(self: Self) -> bool: ...
    def is_zero(self: Self) -> bool: ...
    def num_terms(self: Self) -> int: ...
    @property
    def terms(self: Self) -> list[Term]: ...
