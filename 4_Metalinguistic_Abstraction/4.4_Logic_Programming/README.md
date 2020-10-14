# 4.4 Logic Programming

## 4.4.1 Deductive Information Retrieval

#### Exercise 4.55:

Give simple queries that retrieve the following information from the data base:

1. all people supervised by Ben Bitdiddle;

2. the names and jobs of all people in the accounting division;

3. the names and addresses of all people who live in Slumerville.

#### Exercise 4.56:

Formulate compound queries that retrieve the following information:

a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

b. all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary;

c. all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.

#### Exercise 4.57:

Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 are not the same person. Using your rule, give queries that find the following:

a. all people who can replace Cy D. Fect;

b. all people who can replace someone who is being paid more than they are, together with the two salaries.

#### Exercise 4.58:

Define a rule that says that a person is a “big shot” in a division if the person works in the division but does not have a supervisor who works in the division.

#### Exercise 4.59:

Ben Bitdiddle has missed one meeting too many. Fearing that his habit of forgetting meetings could cost him his job, Ben decides to do something about it. He adds all the weekly meetings of the firm to the Microshaft data base by asserting the following:

```scheme
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
```

Each of the above assertions is for a meeting of an entire division. Ben also adds an entry for the company-wide meeting that spans all the divisions. All of the company’s employees attend this meeting.

```scheme
(meeting whole-company (Wednesday 4pm))
```

a. On Friday morning, Ben wants to query the database for all the meetings that occur that day. What query should he use?

b. Alyssa P. Hacker is unimpressed. She thinks it would be much more useful to be able to ask for her meetings by specifying her name. So she designs a rule that says that a person’s meetings include all `whole-company` meetings plus all meetings of that person’s division. Fill in the body of Alyssa’s rule.

```scheme
(rule (meeting-time ?person ?day-and-time)
      ⟨rule-body⟩)
```

c. Alyssa arrives at work on Wednesday morning and wonders what meetings she has to attend that day. Having defined the above rule, what query should she make to find this out?

#### Exercise 4.60:

By giving the query

```scheme
(lives-near ?person (Hacker Alyssa P))
```

Alyssa P. Hacker is able to find people who live near her, with whom she can ride to work. On the other hand, when she tries to find all pairs of people who live near each other by querying

```scheme
(lives-near ?person-1 ?person-2)
```

she notices that each pair of people who live near each other is listed twice; for example,

```scheme
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))
```

Why does this happen? Is there a way to find a list of people who live near each other, in which each pair appears only once? Explain.

#### Exercise 4.61:

The following rules implement a `next-to` relation that finds adjacent elements of a list:

```scheme
(rule (?x next-to ?y in (?x ?y . ?u)))
(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))
```

What will the response be to the following queries?

```scheme
(?x next-to ?y in (1 (2 3) 4))
(?x next-to  1 in (2 1 3 1))
```

#### Exercise 4.62:

Define rules to implement the `last-pair` operation of Exercise 2.17, which returns a list containing the last element of a nonempty list. Check your rules on queries such as (last-pair (3) ?x), (last-pair (1 2 3) ?x) and (last-pair (2 ?x) (3)). Do your rules work correctly on queries such as (last-pair ?x (3)) ?

#### Exercise 4.63:

The following data base (see Genesis 4) traces the genealogy of the descendants of Ada back to Adam, by way of Cain:

```scheme
(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)
```

Formulate rules such as “If S is the son of f, and f is the son of G, then S is the grandson of G” and “If W is the wife of M, and S is the son of W, then S is the son of M”(which was supposedly more true in biblical times than today) that will enable the query system to find the grandson of Cain; the sons of Lamech; the grandsons of Methushael. (See Exercise 4.69 for some rules to deduce more complicated relationships.)

## 4.4.2 How the Query System Works

## 4.4.3 Is Logic Programming Mathematical Logic?

#### Exercise 4.64:

Louis Reasoner mistakenly deletes the `outranked-by` rule (Section 4.4.1) from the data base. When he realizes this, he quickly reinstalls it. Unfortunately, he makes a slight change in the rule, and types it in as

```scheme
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person
                           ?middle-manager))))
```

Just after Louis types this information into the system, DeWitt Aull comes by to find out who outranks Ben Bitdiddle. He issues the query

```scheme
(outranked-by (Bitdiddle Ben) ?who)
```

After answering, the system goes into an infinite loop. Explain why.

`(outranked-by ?middle-manager ?boss)` is same as `(outranked-by ?staff-person ?boss)`. Apply the second rule of `and` first won't generate infinite loop because `?middle-manager` will be extended.

#### Exercise 4.65:

Cy D. Fect, looking forward to the day when he will rise in the organization, gives a query to find all the wheels (using the `wheel` rule of Section 4.4.1):

```scheme
(wheel ?who)
```

To his surprise, the system responds

```scheme
;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
```

Why is Oliver Warbucks listed four times?

#### Exercise 4.66:

Ben has been generalizing the query system to provide statistics about the company. For example, to find the total salaries of all the computer programmers one will be able to say

```scheme
(sum ?amount (and (job ?x (computer programmer))
                  (salary ?x ?amount)))
```

In general, Ben’s new system allows expressions of the form

```scheme
(accumulation-function ⟨variable⟩ ⟨query pattern⟩)
```

where `accumulation-function` can be things like `sum`, `average`, or `maximum`. Ben reasons that it should be a cinch to implement this. He will simply feed the query pattern to `qeval`. This will produce a stream of frames. He will then pass this stream through a mapping function that extracts the value of the designated variable from each frame in the stream and feed the resulting stream of values to the accumulation function. Just as Ben completes the implementation and is about to try it out, Cy walks by, still puzzling over the `wheel` query result in Exercise 4.65. When Cy shows Ben the system’s response, Ben groans, “Oh, no, my simple accumulation scheme won’t work!”

What has Ben just realized? Outline a method he can use to salvage the situation.

If the query contains `wheel` then it's results will have duplicated data. Track the person from the result and remove the duplicated person.

#### Exercise 4.67:

Devise a way to install a loop detector in the query system so as to avoid the kinds of simple loops illustrated in the text and in Exercise 4.64. The general idea is that the system should maintain some sort of history of its current chain of deductions and should not begin processing a query that it is already working on. Describe what kind of information (patterns and frames) is included in this history, and how the check should be made. (After you study the details of the query-system implementation in Section 4.4.4, you may want to modify the system to include your loop detector.)

Track rule name and it's variables.

#### Exercise 4.68:

Define rules to implement the `reverse` operation of Exercise 2.18, which returns a list containing the same elements as a given list in reverse order. (Hint: Use `append-to-form`.) Can your rules answer both `(reverse (1 2 3) ?x)` and `(reverse ?x (1 2 3))`?

#### Exercise 4.69:

Beginning with the data base and the rules you formulated in Exercise 4.63, devise a rule for adding “greats” to a grandson relationship. This should enable the system to deduce that Irad is the great-grandson of Adam, or that Jabal and Jubal are the great-great-great-great-great-grandsons of Adam. (Hint: Represent the fact about Irad, for example, as `((great grandson) Adam Irad)`. Write rules that determine if a list ends in the word `grandson`. Use this to express a rule that allows one to derive the relationship `((great . ?rel) ?x ?y)`, where `?rel` is a list ending in `grandson`.) Check your rules on queries such as ((great grandson) ?g ?ggs)`` and `(?relationship Adam Irad)`.

## 4.4.4 Implementing the Query System

### 4.4.4.1 The Driver Loop and Instantiation

### 4.4.4.2 The Evaluator

### 4.4.4.3 Finding Assertions by Pattern Matching

### 4.4.4.4 Rules and Unification

### 4.4.4.5 Maintaining the Data Base

#### Exercise 4.70:

What is the purpose of the `let` bindings in the procedures `add-assertion!` and `add-rule!`? What would be wrong with the following implementation of `add-assertion!`? Hint: Recall the definition of the infinite stream of ones in Section 3.5.2: `(define ones (cons-stream 1 ones))`.

```scheme
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)
```

### 4.4.4.6 Stream Operations

### 4.4.4.7 Query Syntax Procedures

### 4.4.4.8 Frames and Bindings

#### Exercise 4.71:

Louis Reasoner wonders why the `simple-query` and `disjoin` procedures (Section 4.4.4.2) are implemented using explicit `delay` operations, rather than being defined as follows:

```scheme
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream))))
```

Can you give examples of queries where these simpler definitions would lead to undesirable behavior?

#### Exercise 4.72:

Why do `disjoin` and `stream-flatmap` interleave the streams rather than simply append them? Give examples that illustrate why interleaving works better. (Hint: Why did we use `interleave` in Section 3.5.3?)
