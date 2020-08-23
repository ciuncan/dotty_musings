package functional

def identity[T](t: T): T = t
def const[T](t: T): Any => T = _ => t
def [A, B, C] (f: A => B => C).flip: B => A => C = b => a => f(a)(b)

trait Functor[F[_]]:
  def [A, B] (f: F[A]).fmap(fun: A => B): F[B]

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
