<!--

Haskell Joinads
===============

This source repository contains the implementation of _joinads_ for the Haskell language.
The design of the extension is described in a paper [Extending Monads with Pattern Matching][1],
which provides more details about the langauge extension and gives numerous examples.

The language extension adds `docase` construct to Haskell. Similarly to the `do` notation,
the construct can be used with any type that provides instance for a certain type class.
In case of `do`, the type class is `Monad`. To support `docase`, type must implement a
`Joinad` type cass that extends `Monad` with three additional operators that represent
non-deterministic _choice_ between computations, _parallel composition_ of computations
and _aliasing_ of computations).

The following example uses the instance for [the `Par` monad][2]. It implements a function
that recursively processes a tree and returns `True` when the tree contains element that 
matches the specified predicate:

    all :: (a -> Bool) -> Tree a -> Par Bool

    all p (Leaf v)           = return (p v)
    all p (Node left right)  = 
      docase (all p left, all p right) of
        (False, ?)    -> return False
        (?, False)    -> return False
        (allL, allR)  -> return (allL && allR)

The `docase` syntax is similar to pattern matching using `case`, but works on monadic computations.
The monadic computation `Par a` represents a task that can be executed (using a parallel scheduler)
and eventually returns a value of type `a`. 

In the above example, the `docase` construct creates a computation that starts processing
left and right sub-trees in parallel. When any of the sub-tasks completes and returns `False`,
we already know that the overall result will be `False`, so we don't need to wait for the
completion of the second task. This expressed using the `?` pattern in the first and the
second clause. Finally, the last clause handles the case when we need to wait for both of the
values before producing the result.

Organization
------------

The `docase` extension is currently implemented using a pre-processor that takes Haskell'98 
code with the `docase` extension and translates it into standard Haskell'98 source (it replaces
`docase` with calls to standard functions). The pre-processor is based on the [pre-processor
for arrow syntax][3] implemented by Ross Paterson.

 * **Preprocessor** directory contains the implementation of the pre-processor. It
   can be compiled using the `Makefile` provided and then executed using 
   the command: `joinadsp.exe Sample.jhs > Sample.hs`

 * **Samples** includes numerous examples that use joinads. It includes `Joinad` instance
   for the `Maybe` type and types mentioned in the paper (parsers that can be used for
   input validation, parallelsim monad used above and simple cooperative parallelism 
   monad using resumptions).

 * **ParMonad** includes core parts of the `Par` monad that is used in the parallel programming
   samples. The implementation is extended to support speculative parallelism (see a separate
   [a blog post][2]). a complete modified implementation is available in a [separate
   project][4]

 * **Comprehensions** shows several examples that use _generalized zip comprehensions_ 
   (or _parallel monad comprehensions_) to write similar examples as those that use
   `docase` notation. Zip comprehensions are a generalization of parallel list comprehensions
   and overlap with joinads. For more information see an article in [The Monad.Reader Issue 18][5].



  [1]: http://www.cl.cam.ac.uk/~tp322/papers/docase.html
  [2]: http://tomasp.net/blog/speculative-par-monad.aspx
  [3]: http://hackage.haskell.org/package/arrowp
  [4]: http://github.com/tpetricek/Haskell.ParMonad
  [5]: http://themonadreader.files.wordpress.com/2011/07/issue18.pdf

-->

<h1>Haskell Joinads</h1>

<p>This source repository contains the implementation of <em>joinads</em> for the Haskell language.
The design of the extension is described in a paper [Extending Monads with Pattern Matching][1],
which provides more details about the langauge extension and gives numerous examples.</p>

<p>The language extension adds <code>docase</code> construct to Haskell. Similarly to the <code>do</code> notation,
the construct can be used with any type that provides instance for a certain type class.
In case of <code>do</code>, the type class is <code>Monad</code>. To support <code>docase</code>, type must implement a
<code>Joinad</code> type cass that extends <code>Monad</code> with three additional operators that represent
non-deterministic <em>choice</em> between computations, <em>parallel composition</em> of computations
and <em>aliasing</em> of computations).</p>

<p>The following example uses the instance for [the <code>Par</code> monad][2]. It implements a function
that recursively processes a tree and returns <code>True</code> when the tree contains element that 
matches the specified predicate:</p>

<pre><code>all :: (a -&gt; Bool) -&gt; Tree a -&gt; Par Bool

all p (Leaf v)           = return (p v)
all p (Node left right)  = 
  docase (all p left, all p right) of
    (False, ?)    -&gt; return False
    (?, False)    -&gt; return False
    (allL, allR)  -&gt; return (allL &amp;&amp; allR)
</code></pre>

<p>The <code>docase</code> syntax is similar to pattern matching using <code>case</code>, but works on monadic computations.
The monadic computation <code>Par a</code> represents a task that can be executed (using a parallel scheduler)
and eventually returns a value of type <code>a</code>. </p>

<p>In the above example, the <code>docase</code> construct creates a computation that starts processing
left and right sub-trees in parallel. When any of the sub-tasks completes and returns <code>False</code>,
we already know that the overall result will be <code>False</code>, so we don't need to wait for the
completion of the second task. This expressed using the <code>?</code> pattern in the first and the
second clause. Finally, the last clause handles the case when we need to wait for both of the
values before producing the result.</p>

<h2>Organization</h2>

<p>The <code>docase</code> extension is currently implemented using a pre-processor that takes Haskell'98 
code with the <code>docase</code> extension and translates it into standard Haskell'98 source (it replaces
<code>docase</code> with calls to standard functions). The pre-processor is based on the [pre-processor
for arrow syntax][3] implemented by Ross Paterson.</p>

<ul style="margin-left:20px">
<li><p><strong>Preprocessor</strong> directory contains the implementation of the pre-processor. It
can be compiled using the <code>Makefile</code> provided and then executed using 
the command: <code>joinadsp.exe Sample.jhs &gt; Sample.hs</code></p></li>
<li><p><strong>Samples</strong> includes numerous examples that use joinads. It includes <code>Joinad</code> instance
for the <code>Maybe</code> type and types mentioned in the paper (parsers that can be used for
input validation, parallelsim monad used above and simple cooperative parallelism 
monad using resumptions).</p></li>
<li><p><strong>ParMonad</strong> includes core parts of the <code>Par</code> monad that is used in the parallel programming
samples. The implementation is extended to support speculative parallelism (see a separate
[a blog post][2]). a complete modified implementation is available in a [separate
project][4]</p></li>
<li><p><strong>Comprehensions</strong> shows several examples that use <em>generalized zip comprehensions</em> 
(or <em>parallel monad comprehensions</em>) to write similar examples as those that use
<code>docase</code> notation. Zip comprehensions are a generalization of parallel list comprehensions
and overlap with joinads. For more information see an article in [The Monad.Reader Issue 18][5].</p></li>
</ul>