import scalariform.formatter.preferences._

scalaVersion := "2.13.12"

scalacOptions ++= Seq(
	"-Xsource:3",                        // Forward source compatibility with Scala 3
	"-unchecked",                        // Show details of unchecked warnings.
	"-deprecation",                      // Show details of deprecation warnings.
	"-feature",                          // Show details of feature warnings.
	"-explaintypes",                     // Explain type errors in more detail.
	"-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
	"-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
	"-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
	"-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
	"-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
	"-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
	"-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
	"-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
	"-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
	"-Xlint:option-implicit",            // Option.apply used implicit view.
	"-Xlint:package-object-classes",     // Class or object defined in package object.
	"-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
	"-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
	"-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
	"-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
	"-Ywarn-dead-code",                  // Fail when dead code is present. Prevents accidentally unreachable code.
	"-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
	"-Ywarn-numeric-widen",              // Warn when numerics are widened.
	"-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
	"-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
	"-Ywarn-unused:locals",              // Warn if a local definition is unused.
	"-Ywarn-unused:params",              // Warn if a value parameter is unused.
	"-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
	"-Ywarn-unused:privates",            // Warn if a private member is unused.
	"-Ywarn-value-discard",              // Warn when non-Unit expression results are unused.
	"-Ymacro-annotations"                // Enable macro annotations
)

scalariformAutoformat := true
scalariformPreferences := scalariformPreferences.value
	.setPreference(IndentWithTabs, true)
	.setPreference(DanglingCloseParenthesis, Preserve)
	.setPreference(DoubleIndentConstructorArguments, true)
	.setPreference(SingleCasePatternOnNewline, false)

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
