SetDifferenceEstimator
======================

Task: Estimate the magnitude of the documents that need to be sychronized between servers.

Assuming we have two servers, with a bounded communication channel, containing a set of documents - we denote these sets with `A` resp. `B`. They are not necessarily disjoint, and we want to estimate the size of the difference, i.e. an approximation of

    m = |A \ B| + |B \ A|.

We have a set of independent hash functions `g₁,..,gₙ` with range `[0,1]` and domain `U`. (That is the domain the sets `A` and `B` are in, i.e. `A ⊆ U, B ⊆ U`.) And a second set of hash functions: `h₁,..,hₙ`: These have domain `P(U)`, i.e. they can be applied to subsets of documents, instead of individual ones, and their range is a b-bit string, i.e.


    gᵢ : U → [0;1]
    hᵢ : P(U) → {0,..,2ᵇ - 1}


for `i ∈ {1,..,n}`.

Instead of sending all documents, both servers send only bn - bits, namely the hashes of

    x(i,A) := h( { x | x ∈ A, gᵢ(x) ≤ pᵢ } )
    x(i,B) := h( { x | x ∈ B, gᵢ(x) ≤ pᵢ } ).

Here we have choosen a sequence of probabilities `p₁,..,pₙ`.

Consider the case when the probability `pᵢ` is so small that only a small portion of the documents in `A` (resp. `B`) are selected for hashing. (Assuming the set difference `m` is small, these sets will likely be equal, and hence their hash value.)

On the other hand, if `m` is big, the probability that the selected set will still be different is very high, even if `pᵢ` is quite small.

The probability of each event can be computed as:

    P(Eᵢ) := P( x(i,A) ≠ x(i,B) ) =      (1-(1-pᵢ)ᵐ) (1-2⁻ᵇ)
    P(Fᵢ) := P( x(i,A) = x(i,B) ) = 1 - [(1-(1-pᵢ)ᵐ) (1-2⁻ᵇ)]

The first term `(1-(1-pᵢ)ᵐ)` in the product `P(Eᵢ)` is the likelihood that there is at least one element from the set `A\B` or `B\A` in the sample, and the second term `1-2⁻ᵇ` is the probability that there is no hash collision.

Given a sequence of observations

     (p_i, x(i,A), x(i,B)) for i = 1, ..., k,

can you estimate the parameter `m` (i.e. the count of distinct elements)?

To be precise, we are looking for the Maximum Likelihood Estimator of `m`, that is the value for `m` - best explaining the observed values for `x(i,A), x(i,B)`.

Note: `P(x(i,A)= x(i,B))` is monotone decreasing with respect to increasing values of `m`, while `P(x(i,A) ≠ x(i,B))` is monotone increasing. Also note that since we assumed the hash functions `hᵢ, gᵢ` to be independent, the probability of an intersection events for `i ≠ j`  will be the product of the probabilities of the individual events, e.g.

    P(Eᵢ ∧ Eⱼ) = P(Eᵢ) P(Eⱼ) 
or

    P(Eᵢ ∧ Fⱼ) = P(Eᵢ) P(Fⱼ) 
and so on ...

    P(Fᵢ ∧ Fⱼ) = P(Fᵢ) * P(Fⱼ) 
as long as `i ≠ j`.

(See also the source file Solution.hs)











