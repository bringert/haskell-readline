--module DirectoryExtTests where

import HUnit
import DirectoryExt

testGetDirFile input output = 
    TestCase $ assertEqual ("getDirFile " ++ show input) (getDirFile input) output

getDirFileTests = [
		   testGetDirFile "" ("",""),
		   testGetDirFile "/" ("/",""),
		   testGetDirFile "/home" ("/","home"),
		   testGetDirFile "/home/" ("/home/",""),
		   testGetDirFile "/home/foo" ("/home/","foo"),
		   testGetDirFile "foo" ("","foo"),
		   testGetDirFile "foo/" ("foo/",""),
		   testGetDirFile "foo/bar" ("foo/","bar"),
		   testGetDirFile "foo/bar/baz" ("foo/bar/","baz"),
		   testGetDirFile "foo/bar/baz/" ("foo/bar/baz/","")
		  ]

testResolvePath base input output = 
    TestCase $ assertEqual ("resolvePath " ++ show base ++ " " ++ show input) (resolvePath base input) output

resolvePathTests = [
		    testResolvePath "/home/foo" "bin" "/home/foo/bin",
		    testResolvePath "/home/foo" "/usr/bin" "/usr/bin"
		   ]


allTests = TestList $ getDirFileTests ++ resolvePathTests

main = runTestTT allTests