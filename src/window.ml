

class c (wo:int) (ho:int) (title:string) =
object (self)
	val mW = wo
	val mH = ho
	val mTitle = title
		
	method w = mW
	method h = mH
	method title = mTitle
	
	
end
