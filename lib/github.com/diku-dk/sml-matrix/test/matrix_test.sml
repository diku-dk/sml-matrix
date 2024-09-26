fun println s = print (s ^ "\n")


structure M = Matrix

fun pr_i s = M.pp 3 Int.toString s
fun pr_r s = M.pp 3 Real.toString s
fun testM pr s e v =
    let val e_s = pr e
        val v_s = pr v
    in if e_s = v_s then println (s ^ ": OK")
       else (println (s ^ ": ERR");
             println (" Expected: ");
             println e_s;
             println (" Got: ");
             println v_s;
             println "")
    end

val m0 =
    M.fromListList [[1,2,3],
                    [4,5,6],
                    [7,8,9]]

val m0t =
    M.fromListList [[1,4,7],
                    [2,5,8],
                    [3,6,9]]

val m0_plus_m1 =
    M.fromListList [[2,6,10],
                    [6,10,14],
                    [10,14,18]]

val m0_sub_m1 =
    M.fromListList [[0,~2,~4],
                    [2,0,~2],
                    [4,2,0]]

val m1 = M.transpose m0

val () = testM pr_i "transpose" m0t m1

val m2 = M.map2 (op+) m0 m1

val () = testM pr_i "plus" m0_plus_m1 m2

val m2a = M.map2 (op-) m0 m1

val () = testM pr_i "sub" m0_sub_m1 m2a

val m3 = M.matmul_gen op* op+ 0 m0 m1

val m3ok = M.fromListList [[14,32,50],[32,77,122],[50,122,194]]

val () = testM pr_i "matmul" m3ok m3

val s = M.pp 3 Int.toString m3

val () = println ("sub: " ^ (if M.sub(m0,1,2) = 6 then "OK" else "ERR"))

val m4 = M.fromListList [[4,7],[2,6]]

val m4 = M.map real m4

val m4inv = M.inv m4

val m4invOk = M.fromListList [[0.6,~0.7],[~0.2,0.4]]

val () = testM pr_r "inv1" m4invOk m4inv

val m5 = M.fromListList [[7,2,1],[0,3,~1],[~3,4,~2]]

val m5 = M.map real m5

val m5inv = M.inv m5

val m5invOk = M.fromListList [[~2.0,8.0,~5.0],[3.0,~11.0,7.0],[9.0,~34.0,21.0]]

val () = testM pr_r "inv2" m5invOk m5inv

val u3 = M.unitmat 3
val u3ok = M.fromListList [[1,0,0],[0,1,0],[0,0,1]]
val () = testM pr_i "unitmat3" u3ok u3

val v1 = Vector.fromList[21.0,5.0,~1.0]

val v2 = M.solve m5 v1

fun prReal r = Real.fmt (StringCvt.FIX(SOME 3)) r

val res = "3.000 1.000 ~2.000"
val () = testM (fn s => s) "solve" res (M.ppv 4 prReal v2)

fun mysolve m v = M.matvecmul (M.inv m) v

val v3 = mysolve m5 v1
val () = testM (fn s => s) "solve2" res (M.ppv 4 prReal v3)

val m6 = M.fromListList [[4,17],[2,6]]

val s = M.pp 2 Int.toString m6
val () =
    print ("pp: " ^ (if s = " 4 17\n 2  6" then "Ok\n"
                     else "Err:\n" ^ s ^ "\n"))
