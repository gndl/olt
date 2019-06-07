
let graphOpen = ref false


class c wo ho title =
  object(self) inherit Window.c wo ho title

    initializer
      (*		let w = maxi wo 50 and h = maxi ho 50*)
      let w = wo and h = ho
      in
      if not !graphOpen then (
	let s = (" "^string_of_int w^"x"^string_of_int h^"+100-1150")
	in
	Graphics.open_graph s;
	Graphics.set_window_title title;
	Graphics.auto_synchronize false;
	graphOpen := true
      )

  end
