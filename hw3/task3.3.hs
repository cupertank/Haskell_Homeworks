func arr = helper arr

helperInit (left:mid:right:other) = helper (mid:right:other) (left + right) 1 2
helperInit arr = -1
helper (left:mid:right:other) maxNum maxPos curPos
	| left + right > maxNum = helper (mid:right:other) (left + right) curPos (curPos + 1)
	| otherwise  		= helper (mid:right:other) maxNum maxPos (curPos + 1)
helper arr maxNum maxPos curPos = maxPos
