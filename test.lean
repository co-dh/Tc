-- All standard collections are homogeneous:
-- #check #[(⟨3, by omega⟩ : Fin 5), (⟨7, by omega⟩ : Fin 10)]  -- Array: ✗
-- #check [(⟨3, by omega⟩ : Fin 5), (⟨7, by omega⟩ : Fin 10)]   -- List: ✗

-- Option 1: Sigma (existential) - wrap value with its type param
def fins1 : Array (Σ n, Fin n) := #[⟨5, 3⟩, ⟨10, 7⟩, ⟨3, 2⟩]
#check fins1  -- ✓

-- Option 2: HList (heterogeneous list) - type-level list
inductive HList : List Type → Type 1 where
  | nil  : HList []
  | cons : α → HList ts → HList (α :: ts)

def fins2 : HList [Fin 5, Fin 10, Fin 3] := .cons 3 (.cons 7 (.cons 2 .nil))
#check fins2  -- ✓

-- Option 3: Sum type (if finite alternatives)
inductive Fin5or10 where
  | f5 : Fin 5 → Fin5or10
  | f10 : Fin 10 → Fin5or10

def fins3 : Array Fin5or10 := #[.f5 3, .f10 7, .f5 1]
#check fins3  -- ✓
