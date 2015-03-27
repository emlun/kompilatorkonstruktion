DD2488 Compiler Construction 2015 project
===

Students: Petter Lundahl <plundahl@kth.se>, Emil Lundberg <emlun@kth.se>

Course home page: http://www.csc.kth.se/~phaller/compilers/


Building
---

Using [Gradle][gradle]:

    $ ./gradlew build

Using SBT:

    $ sbt compile


Running
---

Using jar file:

    $ ./gradlew jar
    $ scala build/libs/koolc-emlun_plundahl-VERSION.jar path/to/source.kool

Using SBT:

    $ sbt 'run path/to/source.kool'


[gradle]: https://gradle.org


Submitting
---

Build the zip file and submit it

    $ ./gradlew test zip
