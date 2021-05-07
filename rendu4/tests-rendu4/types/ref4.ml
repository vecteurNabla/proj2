let f =
    let r = ref [] in
    fun x ->
        r := [x] ;;
f 1 , f true
