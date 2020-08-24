package functional

def identity[T](t: T): T = t
def const[T](t: T): Any => T = _ => t
def [A, B, C] (f: A => B => C).flip: B => A => C = b => a => f(a)(b)

trait Functor[F[_]]:
  def [A, B] (f: F[A]).fmap(fun: A => B): F[B]
  def [A, B] (f: F[A]). map(fun: A => B): F[B] = f fmap fun

trait Prod[F[_]]:
  def [A, B] (fa: F[A]).product(fb: F[B]): F[(A, B)]

trait Applicative[F[_]: Functor: Prod]:
  def [A]             (a: A).pure          : F[A]
  def [A, B] (ff: F[A => B]).ap  (fa: F[A]): F[B]  = ff product fa fmap { case (f, a) => f(a) }
  def [A, B] (ff: F[A => B]) <*> (fa: F[A]): F[B]  = ff ap fa
  def [A, B]     (f: A => B) <|> (fa: F[A]): F[B]  = f.pure <*> fa

trait Monad[M[_]: Applicative]:
  def [A, B]     (m: M[A]).bind   (f: A => M[B]): M[B]
  def [A]    (mm: M[M[A]]).join                 : M[A] = mm bind identity
  def [A, B]    (ma: M[A]).andThen(mb: M[B])    : M[B] = ma bind const(mb)
  def [A, B]     (m: M[A]).flatMap(f: A => M[B]): M[B] =  m bind f
  def [A, B]    (ma: M[A]) >>     (mb: M[B])    : M[B] = ma andThen mb
  def [A, B]     (m: M[A]) >>=    (f: A => M[B]): M[B] =  m bind f

trait Monoid[A]:
  def mempty[A]               : A 
  def [A] (a: A).mappend(b: A): A 
  def [A] (a: A) <+>    (b: A): A = a.mappend(b)

trait Foldable[M[_]]:
  def [A, B] (ma: M[A]).foldl   (b: B)      (f: (B, A) => B)              : B
  def [A, B] (ma: M[A]).foldr   (b: B)      (f: (A, B) => B)              : B
  def [A]    (ma: M[A]).mconcat             (using m: Monoid[A])          : A = ma.foldr(m.mempty)(_ <+> _)
  def [A, B] (ma: M[A]).foldMap (f: A => B) (using Functor[M], Monoid[B]) : B = ma.fmap(f).mconcat


type StateM[S] = [T] =>> S => (S, T)
object StateM:
  def    get[S]           : StateM[S][S]    = s => (s, s)
  def    set[S](newS: S)  : StateM[S][Unit] = _ => (newS, ())
  def modify[S](f: S => S): StateM[S][Unit] = s => (f(s), ())

given stateFunctor[S] as Functor[StateM[S]]:
  type SS[A] = StateM[S][A]
  def [A, B] (s: SS[A]).fmap(fun: A => B): SS[B] = first => s(first) match
    case (newState, a) => (newState, fun(a))

given stateProduct[S] as Prod[StateM[S]]:
  def [A, B] (sa: StateM[S][A]).product(sb: StateM[S][B]): StateM[S][(A, B)] = s => sa(s) match
    case (newS, a) => sb(newS) match
      case (newNewS, b) => (newNewS, a -> b)

given stateApplicative[S] as Applicative[StateM[S]]:
  def [A](a: A).pure: StateM[S][A] = s => (s, a)

given stateMonad[S] as Monad[StateM[S]]:
  def [A, B] (sa: StateM[S][A]).bind(f: A => StateM[S][B]): StateM[S][B] = s => sa(s) match
    case (newS, a) => f(a)(newS)

extension [S, A] (sm: StateM[S][A]):
  def repeat(n: Int): StateM[S][A] =
    if n <= 0
    then sm
    else sm >> sm.repeat(n - 1)

given Functor[Option]:
  def [A, B] (oa: Option[A]).fmap(f: A => B): Option[B] = oa.map(f)

given Prod[Option]:
  def [A, B] (fa: Option[A]).product(fb: Option[B]): Option[(A, B)] = for
    a <- fa
    b <- fb
  yield a -> b

given Applicative[Option]:
  def [A] (a: A).pure: Option[A] = Some(a)

given Monad[Option]:
  def [A, B] (m: Option[A]).bind(f: A => Option[B]): Option[B] = m.flatMap(f)

given Foldable[Option]:
  def [A, B] (oa: Option[A]).foldl(b: B)(f: (B, A) => B): B = oa.fold(b)(a => f(b, a))
  def [A, B] (oa: Option[A]).foldr(b: B)(f: (A, B) => B): B = oa.fold(b)(a => f(a, b))
