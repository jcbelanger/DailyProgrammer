{-
[2015-02-09] Challenge #201 [Easy] Counting the Days until...

http://www.reddit.com/r/dailyprogrammer/comments/2vc5xq/20150209_challenge_201_easy_counting_the_days/

Description:

Sometimes you wonder. How many days I have left until.....Whatever date you are curious about. Maybe a holiday. Maybe a vacation. Maybe a special event like a birthday.
So today let us do some calendar math. Given a date that is in the future how many days until that date from the current date?
Input:

The date you want to know about in 3 integers. I leave it to you to decide if you want to do yyyy mm dd or mm dd yyyy or whatever. For my examples I will be using yyyy mm dd. Your solution should have 1 comment saying what format you are using for people reading your code. (Note you will need to convert your inputs to your format from mine if not using yyyy mm dd)
Output:

The number of days until that date from today's date (the time you run the program)
Example Input: 2015 2 14

Example Output: 5 days from 2015 2 9 to 2015 2 14

Challenge Inputs:

 2015 7 4
 2015 10 31
 2015 12 24
 2016 1 1
 2016 2 9
 2020 1 1
 2020 2 9
 2020 3 1
 3015 2 9
Challenge Outputs:

Vary from the date you will run the solution and I leave it to you all to compare results.
-}

import Data.Time
import System.Locale
import Text.Printf

main = do
    today <- fmap utctDay getCurrentTime
    let dateFormat = "%Y %-m %-d"
        parseDay = readTime defaultTimeLocale dateFormat
        showDay = formatTime defaultTimeLocale dateFormat
        challenge target = printf "%d days from %s to %s" (diffDays target today) (showDay today) (showDay target)
    interact $ challenge . parseDay