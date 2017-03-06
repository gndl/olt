
class c =
	object(self)
	val mutable mActions : (int -> int -> unit) list = []

	method apply a b = List.iter(fun f -> f a b) mActions

  method getActions = mActions
  method addAction f = mActions <- mActions @ [f]
  method removeAction f = mActions <- List.filter(fun a -> a != f) mActions
  method setAction f = mActions <- [f]
  method resetActions = mActions <- []
end