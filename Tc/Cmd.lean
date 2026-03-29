/-
  Effect types for pure code that can't do IO.
  PlotKind, ExportFmt, Effect.
-/

inductive PlotKind where | line | bar | scatter | hist | box | area | density | step | violin deriving Repr, BEq

instance : ToString PlotKind where
  toString | .line => "line" | .bar => "bar" | .scatter => "scatter" | .hist => "hist" | .box => "box"
           | .area => "area" | .density => "density" | .step => "step" | .violin => "violin"
inductive ExportFmt where | csv | parquet | json | ndjson deriving Repr, BEq

-- | Residual effects from pure code that can't do IO (View.update, ViewStack.update, Freq.update).
-- Most effects were eliminated by having dispatch call IO directly.
inductive Effect where
  | none | quit | fetchMore
  | sort (colIdx : Nat) (sels : Array Nat) (grp : Array Nat) (asc : Bool)
  | exclude (cols : Array String)
  | freq (colNames : Array String)
  | freqFilter (cols : Array String) (row : Nat)
  deriving Repr, BEq

namespace Effect
def isNone : Effect → Bool | .none => true | _ => false
end Effect
