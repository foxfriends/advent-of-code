# Advent of Code 2022

I return with a new language per day again. Maybe this year I'll remember to keep doing them.

1212704-0a296724

## Reflection

25 languages is... a lot of languages. Started with ones I was familiar with but
quickly ran out and was forced to try some I had barely/never seen before.

I tried to use something that was relatively unique to each, otherwise why bother
keep changing, and found in most of them, particularly the ones I hadn't yet used,
something new to learn, which was fun.

1.  Not much to say about Bash. Got slightly tripped up by the numerical context, but mostly just because of the syntax.
2.  Prolog is always a fun one for generating a finite number of states. Summing up the list remains less pretty in that it cannot be reversed as easily, breaking the illusion.
3.  I never do like Python, but they have sets with all the methods needed. Had never used the explicit `iter` or `next` before, kind of nice to know about.
4.  C has very few redeeming features these days, almost anything it can do, other languages can do better. `scanf` is one of the few features that doesn't get dragged along though, a safer variant could be a nice addition to modern languages.
5.  Perl is interesting in how implicit things can be. Sure makes it hard to follow, but fun to write.
6.  The Ruby one-liner didn't give me much time to explore anything interesting about the language, but I was disappointed to find that `Set` needed to be imported.
7.  IO's prototypal inheritance could probably have been nice, though I'm not sure it was all that practical for this problem. Many ideas of this language I enjoy, but when it comes time to write anything I always find it kind of awkward.
8.  C++ was not a good time. I remember sort of liking it when I was younger, but now it just feels awkward and like things are missing. I probably am just falling behind on the latest, so I ended up just copy pasting loops around, like I did when I was younger.
9.  Clojure was the first time I was pleasantly surprised by one of these, and also the first language I was not particularly familiar with before starting. Had not seen multimethods like this before, so that was fun to play with and led to some pretty concise code. Would use again, if not for the JVM.
10. Kotlin does not feel like it has matured since I first learned it, so remains a bit lacking to me. Like it's nearly functional, but missing patterns. Lucky I didn't need any, and for list iteration it works great.
11. This was probably not a great problem for Scala, I ended up using the OOP parts, but not the functional parts all that much. The large quantities of syntax sugar are a lot of fun though, so Scala can keep top spot of JVM languages.
12. Haskell was almost certainly the wrong choice for this problem. I was hoping to use some monads or something, but with my feeble attempts they actually just convoluted everything and regular code won out. Maybe that's the story of life in Haskell though.
13. Javascript (in a TypeScript file) really showed me nothing new, but I went with Deno to at least make it a little interesting. Top level `await` (and `for await`) makes reading input in JS nicer than it was years back.
14. Zig's compile time powers make for some interesting techniques, but I don't think they showed themselves well in this situation, only in error messages. I do like the error handling pattern, but the rest of the language just doesn't feel ready for any practical purpose.
15. I've done ML before, but not specificall OCaml until now. I really can't tell the difference though. There's some good stuff here, the modules are fun, but I am not sure what they bring over generic types/interfaces. Maybe this simple example wasn't enough to really see it.
16. Go impressed me with how much I disliked it. I thought Go would be a decent language with all the traction it has, but the only feature it has is for loops. If not building entirely based on channels, don't use Go, it's just not fun.
17. Swift felt surprisingly clunky for this one, but maybe I just didn't like the question. String indexing is so annoying the way they've implemented it... Correct, but annoying.
18. I had been joking about doing one in SQL, but this problem... actually was very elegant in SQL. Part 1 anyway. Part 2 was where I learned just how much more SQL could do than I had ever used it for before. Recursive views are pretty neat. When does any actual application benefit from such an extravagant database is the real question though.
19. Rust of course is a good time. I had initially written this solution in Erlang but it was so unbearably slow (or there was a bug?). When ported to Rust, same algorithm, it solved almost instantly. It just works :tm:. Admittedly a little verbose for a small script though.
20. Gave Erlang a second attempt today and it was much preferable. The Prolog syntax but non-Prolog semantics threw me off, but with pattern matching (nearly) as powerful as Prolog, I kind of enjoyed it.
21. Wildcarded [Noulith](https://github.com/betaveros/noulith) in there, and was impressed. Nicely done by betaveros. Not sure I agree with the "no braces, just more parentheses" choice, but many of the other features I found very intuitive and a lot of fun. Inspired me to write my own language (coming soon) for which I might steal a few ideas from Noulith (among others I saw this month).
22. For all the hype Elixir had been given, I was mildly disappointed by it. The Ruby syntax on a functional language, it kind of clashes in the mind. I would probably pick Erlang over Elixir if not for all the libraries.
23. Not sure what Nim's deal is... what is it trying to achieve? Was interesting what could be done with macros (`collect` and `std/sugar` are neat), and iterator syntax was fun (although, isn't it just a generator?), but when it turns out `array = [array[1], array[0]]` does not successfully swap the two values, I am left feeling the language is buggy (or unintuitive at best).
24. Crystal was surprisingly nice, it really did feel like Ruby on a compiler, bells and whistles included. Rare to see `x.lcm(y)` being in the standard library, particularly in a compiled language, that's usually a whole algorithm you have to write. The automatic union types are fun too, though they can be a surprise when not intending to return one of two things.
25. And finally PHP, I sadly cannot say is a great language, `array_map` takes the array as second param, but `array_reduce` takes it as first, and it's generally inconsistent all over. `array_flip` was a nice surprise for this particular problem.
