import AssignmentsAndSubmissions
import Data.Time.Clock


-- | Values for testing
time = (read "2016-01-01 00:00:01 UTC")::UTCTime
a1 = Assignment 2015 Homework 5
a2 = Assignment 2015 Homework 6
c1 = Configuration time time time ["Homework.hs", "Exercise.hs"] 0 10 5


main = do
	createAssignment a1 c1 "/home/dan/test.pdf"
	getConfiguration a1
	upload a1 "Branko" "test\ntest" "Homework.hs"
	getSubmission a1 "Branko"
	listSubmissions a1
