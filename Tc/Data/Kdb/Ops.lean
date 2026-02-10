/-
  TblOps/ModifyTable instances for KdbTable
  Used by Full build only
-/
import Tc.Data.Kdb.Table
import Tc.Render

namespace Tc

-- | Build q filter expression from fzf result
-- q uses = (not ==), backtick symbols, `in` for multiple values
private def buildFilterQ (_t : KdbTable) (col : String) (vals : Array String)
    (result : String) (numeric : Bool) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  let input := lines.getD 0 ""
  let fromHints := (lines.extract 1 lines.size).filter vals.contains
  let selected := if vals.contains input && !fromHints.contains input
                  then #[input] ++ fromHints else fromHints
  if selected.size == 1 then
    let v := selected.getD 0 ""
    if numeric then s!"{col}={v}" else s!"{col}=`{v}"
  else if selected.size > 1 then
    if numeric then
      let joined := selected.toList |> String.intercalate " "
      s!"{col} in ({joined})"
    else
      let joined := String.join (selected.map fun v => s!"`{v}").toList
      s!"{col} in {joined}"
  else if !input.isEmpty then input
  else ""

-- | TblOps instance for KdbTable
instance : TblOps KdbTable where
  nRows     := (·.nRows)
  colNames  := (·.colNames)
  totalRows := (·.totalRows)
  filter    := KdbTable.filter
  distinct  := KdbTable.distinct
  findRow   := KdbTable.findRow
  getCols t idxs r0 r1 := idxs.mapM fun i => t.getCol i r0 r1
  colType t col := match t.colTypes.getD col '?' with
    | 'j' | 'i' | 'h' => "int" | 'f' | 'e' => "float"
    | 't' => "time" | 'p' | 'z' => "timestamp" | 'd' => "date"
    | _ => "str"
  buildFilter := buildFilterQ
  filterPrompt := fun _ col => s!"{col}: {col}>5 | {col}=`val > "
  render t ctx := do
    let r1' := min ctx.r1 (ctx.r0 + maxVisRows)
    let cols ← (Array.range t.nCols).mapM fun i => t.getCol i ctx.r0 r1'
    renderCols cols t.colNames t.colTypes t.nRows ctx ctx.r0 (r1' - ctx.r0)

-- | ModifyTable instance for KdbTable
instance : ModifyTable KdbTable where
  delCols := fun delIdxs t => KdbTable.delCols t delIdxs
  sortBy  := fun idxs asc t => KdbTable.sortBy t idxs asc

end Tc
