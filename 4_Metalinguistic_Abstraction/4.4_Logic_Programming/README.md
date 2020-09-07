# 4.4 Logic Programming

## 4.4.1 Deductive Information Retrieval

### Exercise 4.55:

Give simple queries that retrieve the following information from the data base:

1. all people supervised by Ben Bitdiddle;

2. the names and jobs of all people in the accounting division;

3. the names and addresses of all people who live in Slumerville.

### Exercise 4.56:

Formulate compound queries that retrieve the following information:

a. the names of all people who are supervised by Ben Bitdiddle, together with their addresses;

b. all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary;

c. all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job.

### Exercise 4.57:

Define a rule that says that person 1 can replace person 2 if either person 1 does the same job as person 2 or someone who does person 1’s job can also do person 2’s job, and if person 1 and person 2 are not the same person. Using your rule, give queries that find the following:

a. all people who can replace Cy D. Fect;

b. all people who can replace someone who is being paid more than they are, together with the two salaries.

### Exercise 4.58:

Define a rule that says that a person is a “big shot” in a division if the person works in the division but does not have a supervisor who works in the division.

### Exercise 4.59:

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

### Exercise 4.60:

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

### Exercise 4.61:

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

### Exercise 4.62:

Define rules to implement the `last-pair` operation of Exercise 2.17, which returns a list containing the last element of a nonempty list. Check your rules on queries such as (last-pair (3) ?x), (last-pair (1 2 3) ?x) and (last-pair (2 ?x) (3)). Do your rules work correctly on queries such as (last-pair ?x (3)) ?

### Exercise 4.63:

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