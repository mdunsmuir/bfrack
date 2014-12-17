main = do interact markPalindromes
markPalindromes str = unlines $ (\s -> s : if isPalindrome s 
                                           then ["palindrome"] 
                                           else ["not palindrome"]) =<< lines str
isPalindrome str = str == reverse str
