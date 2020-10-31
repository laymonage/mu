# Tugas 5 Pemrograman Fungsional: (Alonzo) Church's Numeral Interpretation

| NPM        | Nama                   |
| ---        | ---------------------- |
| 1706979455 | Sage Muhammad Abdullah |

Repositori ini berisi pekerjaan Tugas 5 Pemrograman Fungsional 2020/2021
Fakultas Ilmu Komputer, Universitas Indonesia.

Pekerjaan ini merupakan *fork* dari repositori [vwzGrey/mu][mu] di GitHub.
Adapun modifikasi yang diterapkan adalah sebagai berikut.

## *Tracing* evaluasi ekspresi lambda per langkah

Sebelum dimodifikasi, program `mu` hanya mencetak hasil akhir dari evaluasi
ekspresi lambda. Supaya proses evaluasi terlihat lebih jelas, kode program
dimodifikasi sehingga menunjukkan proses evaluasi pada setiap langkah.
Modifikasi dilakukan dengan mengubah fungsi `reduce` pada
[`app/Mu/Evaluator.hs`](app/Mu/Evaluator.hs) sebagai berikut.

```diff
diff --git a/app/Mu/Evaluator.hs b/app/Mu/Evaluator.hs
index cf647a0..19a1628 100644
--- a/app/Mu/Evaluator.hs
+++ b/app/Mu/Evaluator.hs
@@ -2,7 +2,10 @@ module Mu.Evaluator (Evaluator, Aliases, evaluate) where

 import Control.Monad.State
 import qualified Data.Map.Strict as M
+import qualified Data.Text as T
+import Debug.Trace
 import Mu.Parser
+import Mu.Util

 alphaConvert :: AST -> Identifier -> AST -> AST
 alphaConvert (Variable n) v r
@@ -31,7 +34,9 @@ reduce :: AST -> AST
 reduce ast =
   let ast' = betaReduce ast
    in if ast /= ast'
-        then reduce ast'
+        then
+          trace (T.unpack (prettyAST ast))
+          reduce ast'
         else ast
```

[mu]: https://github.com/vzwGrey/mu
