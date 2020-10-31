# Tugas 5 Pemrograman Fungsional: (Alonzo) Church's Numeral Interpretation

| NPM        | Nama                   |
| ---        | ---------------------- |
| 1706979455 | Sage Muhammad Abdullah |

Repositori ini berisi pekerjaan Tugas 5 Pemrograman Fungsional 2020/2021
Fakultas Ilmu Komputer, Universitas Indonesia.

Pekerjaan ini merupakan *fork* dari repositori [**vwzGrey/mu**][mu] di GitHub.
Adapun modifikasi yang diterapkan adalah sebagai berikut.

## *Tracing* evaluasi ekspresi lambda per langkah

Sebelum dimodifikasi, program `mu` hanya mencetak hasil akhir dari evaluasi
ekspresi lambda. Supaya proses evaluasi terlihat lebih jelas, kode program
dimodifikasi sehingga menunjukkan proses evaluasi pada setiap langkah.
Modifikasi dilakukan dengan mengubah fungsi `reduce` pada
[`app/Mu/Evaluator.hs`](app/Mu/Evaluator.hs) supaya memanggil fungsi `trace`
dari library `Debug.Trace` setiap kali melakukan *beta reduction*.

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

## Definisi alias untuk Church's Numerals dan ekspresi lainnya

Untuk memudahkan penggunaan, beberapa alias telah dibuat untuk ekspresi lambda
yang umum digunakan, yakni `S`, `+`, `*`, `0` sampai `9`, `T`, `F`, `True`,
`False`, `not`, `and`, `or`, dan `Z`.

Untuk mengimplementasikan alias tersebut supaya tersedia ketika program
dijalankan, dilakukan modifikasi pada kode program. Beberapa di antara
modifikasi tersebut adalah sebagai berikut.

### Penambahan fungsi `runSilent`

Untuk memudahkan penambahan alias pada `Map` yang ada di program, dibuat
fungsi `runSilent` yang dapat mengevaluasi input tanpa konteks `IO`.
Fungsi `runSilent` disadur dari fungsi `run` dan didefinisikan sebagai berikut.

```haskell
-- | Evaluates an input silently (without printing the final result).
runSilent :: Aliases -> T.Text -> Aliases
runSilent as source =
  case runParser program "repl" source of
    Left _ -> as
    Right exprs -> do
      let (_, as') = runState (sequence $ map evaluate exprs) as
      as'
```

### Penambahan *list* `initialInputs`

Alias dibuat dengan membuat *list* yang berisi `String` input, lalu
memetakannya menjadi *list* yang berisi `Text`, yang kemudian dievaluasi satu
per satu dengan menerapkan fungsi `runSilent` menggunakan `foldl`. Hasil
akumulasi `foldl` yang berupa `Map` (`Aliases`) dijadikan sebagai *environment*
awal untuk pemanggilan fungsi `repl` pada fungsi `main`.

```haskell
-- | Predefined inputs to be evaluated in the environment.
initialInputs :: [String]
initialInputs =
  [ "S     := λw.λy.λx.y(w y x)"
  , "+     := S"
  , "*     := λx.λy.λz.x(y z)"
  , ... -- and so on.
  ]

-- | Evaluates a list of Text in the environment.
initialEnvironment :: Foldable t => t T.Text -> Aliases
initialEnvironment = foldl runSilent M.empty
```

### Pemastian *flushing* evaluasi `initialInputs`

Haskell mengevaluasi `initialInputs` secara *lazy* sehingga *trace* hasil
evaluasinya baru muncul ketika pengguna memberikan suatu input berupa ekspresi.
Untuk memastikan bahwa `initialInputs` telah dievaluasi dan *trace*-nya sudah
dicetak, program dibuat agar mengevaluasi string `Initialization finished` pada
awal program sebelum memanggil fungsi `repl`.

```haskell
-- | Force the initialization to be evaluated and the traces to be flushed.
replEntry :: Aliases -> IO ()
replEntry as = do
  _ <- run as $ T.pack "Initialization finished"
  repl as

initialMessage :: String
initialMessage = "Initializing predefined aliases..."

main :: IO ()
main = do
  putStrLn(initialMessage)
  replEntry $ initialEnvironment $ map T.pack initialInputs
```

## Lain-lain

Berkas [`stack.yml`](stack.yaml) juga ditambahkan ke repositori ini untuk
memudahkan instalasi menggunakan [Stack][stack]. Untuk lebih lengkapnya
mengenai proyek `mu`, silakan baca [`README.mu.md`](README.mu.md).

[mu]: https://github.com/vzwGrey/mu
[stack]: https://docs.haskellstack.org
