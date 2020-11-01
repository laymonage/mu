# Tugas 5 Pemrograman Fungsional: (Alonzo) Church's Numeral Interpretation

1706979455\
Sage Muhammad Abdullah

Repositori ini berisi pekerjaan Tugas 5 Pemrograman Fungsional 2020/2021
Fakultas Ilmu Komputer, Universitas Indonesia. Pekerjaan ini merupakan *fork*
dari repositori [**vwzGrey/mu**][mu] di GitHub. Adapun modifikasi yang
diterapkan adalah sebagai berikut.

## *Tracing* evaluasi ekspresi lambda per langkah

Sebelum dimodifikasi, program `mu` hanya mencetak hasil akhir dari evaluasi
ekspresi lambda. Supaya proses evaluasi terlihat lebih jelas, kode program
dimodifikasi sehingga menunjukkan proses evaluasi pada setiap langkah.
Modifikasi dilakukan dengan mengubah fungsi `reduce` pada
[`app/Mu/Evaluator.hs`](app/Mu/Evaluator.hs) supaya memanggil fungsi `trace`
dari library `Debug.Trace` setiap kali melakukan *beta reduction*.

```diff
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
`False`, `not`, `and`, `or`, `Z`, dan `ifthenelse`.

Untuk mengimplementasikan alias tersebut supaya tersedia ketika program
dijalankan, dilakukan modifikasi pada kode [`app/Main.hs`](app/Main.hs) dan
[`app/Mu/Parser.hs`](app/Mu/Parser.hs).
Beberapa di antara modifikasi tersebut adalah sebagai berikut.

### Penambahan `+` dan `*` pada *lexeme* untuk *parser* *identifier*.

Program `mu` pada awalnya hanya dapat menerima string alfanumerik sebagai
*identifier*. Agar alias `+` dan `*` dapat dibuat, kedua karakter tersebut
ditambahkan ke dalam *lexeme* untuk *parser* *identifier*. *Lexeme* ini
didefinisikan pada fungsi `aliasIdent`.

```diff
 -- | Parse an identifier for an alias.
 aliasIdent :: Parser Identifier
- aliasIdent = T.pack <$> lexeme (M.some alphaNumChar)
+ aliasIdent = T.pack <$> lexeme (M.some $ alphaNumChar <|> char '+' <|> char '*')
```

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

### Pemastian *flushing* evaluasi `initialInputs`

Haskell mengevaluasi `initialInputs` secara *lazy* sehingga *trace* hasil
evaluasinya baru muncul ketika pengguna memberikan suatu input berupa ekspresi.
Untuk memastikan bahwa `initialInputs` telah dievaluasi dan *trace*-nya sudah
dicetak, dibuat fungsi `replEntry` agar mengevaluasi string
`Initialization finished` pada awal program sebelum memanggil fungsi `repl`.

## Konversi Church's Numerals kembali ke digit desimal

Untuk kemudahan membaca keluaran, Church's Numerals yang dicetak ke layar juga
dicetak dalam bentuk digit desimal. Fitur ini diimplementasikan dengan
**sangat naif**, yakni dengan menghitung kemunculan `(y ` dalam `Text` yang
akan dicetak. Selain itu, jika bilangan merupakan hasil perkalian, maka yang
dihitung adalah kemunculan `z ` dalam `Text` yang akan dicetak. Akibat naifnya
implementasi ini, mungkin akan muncul *bug* di mana ada bilangan dalam digit
desimal yang dicetak, padahal ekspresinya bukan merupakan bilangan dalam
Church's Numerals.

## Lain-lain

Berkas [`stack.yml`](stack.yaml) juga ditambahkan ke repositori ini untuk
memudahkan instalasi menggunakan [Stack][stack]. Untuk lebih lengkapnya
mengenai proyek `mu`, silakan baca [`README.mu.md`](README.mu.md).

[mu]: https://github.com/vzwGrey/mu
[stack]: https://docs.haskellstack.org
