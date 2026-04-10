# Haskell Lenses: How Would You Invent Them?

This walks through the reasoning that leads from a concrete, annoying
problem to the modern `van Laarhoven` lens definition. The goal is not
to memorize the type, but to feel why each step is forced.

## 1. The Problem: Nested Immutable Updates

In Haskell, records are immutable. To "change" a field you build a new
value. That's fine for one level:

```haskell
data Person = Person { name :: String, age :: Int }

birthday :: Person -> Person
birthday p = p { age = age p + 1 }
```

Now nest them:

```haskell
data Address = Address { street :: String, city :: String }
data Person  = Person  { name :: String, addr :: Address }
data Company = Company { boss :: Person, staff :: [Person] }
```

Updating the boss's street is already painful:

```haskell
moveBoss :: String -> Company -> Company
moveBoss s c =
  c { boss = (boss c)
        { addr = (addr (boss c))
            { street = s } } }
```

Notice the pathological shape: **to update deep inside, you have to
rebuild every enclosing record by hand.** It doesn't compose, and the
field name `addr` appears once as a "getter" and once as a "setter".

We want something first-class that represents "a focus on a field
inside a structure" and that **composes**.

## 2. First Attempt: A Pair of Functions

The obvious abstraction: a lens from `s` to `a` is a getter and a
setter:

```haskell
data Lens s a = Lens
  { view :: s -> a
  , set  :: a -> s -> s
  }
```

Now we can write:

```haskell
addrL   :: Lens Person Address
streetL :: Lens Address String
```

But how do you **compose** them to get `Lens Person String`?

```haskell
compose :: Lens s a -> Lens a b -> Lens s b
compose (Lens g1 s1) (Lens g2 s2) = Lens
  { view = g2 . g1
  , set  = \b s -> s1 (s2 b (g1 s)) s
  }
```

It works! But two problems:

1. The `set` definition is fiddly (`g1 s` is called, then `s2 ... `
   threads through `s1`). Easy to get wrong.
2. What about **modify**? We often want `f :: a -> a` applied deep
   inside. We'd need a third field `over :: (a -> a) -> s -> s`, and
   the composition gets even worse.

So we add `over`:

```haskell
data Lens s a = Lens
  { view :: s -> a
  , over :: (a -> a) -> s -> s
  }
```

And `set b = over (const b)`. But now `view` and `over` feel
redundant — they both "know" how to drill into `s`. There must be a
way to express them as **one** function.

## 3. The Unification Insight

Look at `over`:

```haskell
over :: (a -> a) -> (s -> s)
```

It says: given a way to transform the focus, give me a way to
transform the whole. That already encodes the *location* of the focus.

Can `view` be recovered from `over`? Almost — if we could apply a
function that, instead of returning a new `a`, **remembers** the `a`
it saw. That's a function `a -> something carrying a`.

This is the key generalization. Replace `a -> a` with `a -> f a` for
some functor `f`, and replace `s -> s` with `s -> f s`:

```haskell
type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)
```

This is the **van Laarhoven** formulation. At first it looks insane.
Watch what it buys us.

## 4. Why This Type Is Both View and Set

A lens is now "a function that, given a way to transform the focus
into some functor, transforms the whole into that same functor". By
**choosing the functor**, we recover every operation:

### `over` — pick `Identity`

```haskell
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where fmap f (Identity a) = Identity (f a)

over :: Lens s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)
```

`Identity` is a trivial wrapper: `a -> Identity a` is just `a -> a`
with a newtype coat. So passing `Identity . f` through the lens
applies `f` at the focus and gives back the updated `s`.

### `set` — same thing

```haskell
set :: Lens s a -> a -> s -> s
set l b = over l (const b)
```

### `view` — pick `Const`

```haskell
newtype Const c a = Const { getConst :: c }
instance Functor (Const c) where fmap _ (Const c) = Const c
```

`Const c` is a functor that **ignores** its type parameter — `fmap`
does nothing. If you push `a -> Const a a` through the lens, the `f s`
it builds will just carry the original `a` out, unchanged, because
every `fmap` along the way is a no-op:

```haskell
view :: Lens s a -> s -> a
view l s = getConst (l Const s)
```

**Both view and set fall out of the same single function, depending
on which functor you feed in.** That's the magic.

## 5. Composition Is Just `(.)`

Because a lens is a function, composing lenses is ordinary function
composition:

```haskell
addrStreetL :: Lens Person String
addrStreetL = addrL . streetL
```

Wait, why does the order look "backwards" (outer then inner)? Because
a `Lens s a` is `(a -> f a) -> (s -> f s)`: it takes the inner
transformer and gives back the outer. So composition chains them:

```
streetL        :: (String  -> f String ) -> (Address -> f Address)
addrL          :: (Address -> f Address) -> (Person  -> f Person)
addrL . streetL :: (String  -> f String ) -> (Person  -> f Person)
```

No more hand-threading `set`s through `view`s. The type system does
the plumbing.

## 6. Writing a Lens By Hand

For a field `addr :: Person -> Address`:

```haskell
addrL :: Lens Person Address
addrL f p = fmap (\a' -> p { addr = a' }) (f (addr p))
```

Read it aloud: "Take the current `addr` out of `p`, hand it to `f` to
get an `f Address`, then `fmap` the rebuild back in." The single
`fmap` is what lets the choice of functor specialize to view/set/modify.

Every record-field lens has this shape. That's why `makeLenses` in
the `lens` library can generate them via Template Haskell.

## 7. Generalizations You Get Almost for Free

Once the "lens = function polymorphic in a functor" trick clicks, you
can vary the functor constraint to get other optics:

| Optic       | Constraint on `f`          | Focuses on           |
|-------------|-----------------------------|----------------------|
| `Lens`      | `Functor`                   | exactly one `a`      |
| `Traversal` | `Applicative`               | zero or more `a`s    |
| `Prism`     | `Applicative` + `Choice`    | possibly-absent `a`  |
| `Iso`       | `Functor` + `Profunctor`    | isomorphic `a`       |
| `Getter`    | `Contravariant` + `Functor` | read-only `a`        |
| `Setter`    | `Settable` (Identity-like)  | write-only `a`       |

All compose with `(.)`. All work together. That's the payoff: one
representation, infinite specializations, free composition.

## 8. Summary — Why Lenses Are Forced

1. Immutable nested updates are syntactically horrible.
2. A (getter, setter) pair works but doesn't compose cleanly and
   duplicates the "drill in" logic.
3. You want one function that knows the location of a field.
4. `over :: (a -> a) -> (s -> s)` isolates the location but loses
   the ability to *read*.
5. Generalize `a -> a` to `a -> f a` for any `Functor f`. Now:
   - Pick `Identity` → you get modify/set.
   - Pick `Const`    → you get view.
6. Because lenses are ordinary functions, `(.)` composes them.
7. Varying the functor constraint gives you the entire optics zoo
   for free.

The van Laarhoven type
`forall f. Functor f => (a -> f a) -> (s -> f s)`
is not clever for cleverness's sake. It's the smallest thing that
unifies reading, writing, and modifying **and** composes with plain
function composition. Once you see that `Identity` gives you one
operation and `Const` gives you the other, the whole design feels
inevitable.
