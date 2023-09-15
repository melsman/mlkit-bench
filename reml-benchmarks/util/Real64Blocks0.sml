
structure Real64Block2 :> REAL64_BLOCK2 = struct
    type t = real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
end

structure Real64Block3 :> REAL64_BLOCK3 = struct
    type t = real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
end

structure Real64Block4 :> REAL64_BLOCK4 = struct
    type t = real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
end

structure Real64Block5 :> REAL64_BLOCK5 = struct
    type t = real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
end

structure Real64Block6 :> REAL64_BLOCK6 = struct
    type t = real*real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
    val sub5 : t -> real = #6
end

structure Real64Block7 :> REAL64_BLOCK7 = struct
    type t = real*real*real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
    val sub5 : t -> real = #6
    val sub6 : t -> real = #7
end

structure Real64Block8 :> REAL64_BLOCK8 = struct
    type t = real*real*real*real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
    val sub5 : t -> real = #6
    val sub6 : t -> real = #7
    val sub7 : t -> real = #8
end

structure Real64Block9 :> REAL64_BLOCK9 = struct
    type t = real*real*real*real*real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
    val sub5 : t -> real = #6
    val sub6 : t -> real = #7
    val sub7 : t -> real = #8
    val sub8 : t -> real = #9
end

structure Real64Block10 :> REAL64_BLOCK10 = struct
    type t = real*real*real*real*real*real*real*real*real*real
    fun pack (x:t) : t = x
    fun unpack (x:t) : t = x
    val sub0 : t -> real = #1
    val sub1 : t -> real = #2
    val sub2 : t -> real = #3
    val sub3 : t -> real = #4
    val sub4 : t -> real = #5
    val sub5 : t -> real = #6
    val sub6 : t -> real = #7
    val sub7 : t -> real = #8
    val sub8 : t -> real = #9
    val sub9 : t -> real = #10
end
