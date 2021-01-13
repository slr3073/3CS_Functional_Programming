
(***************************************** peintures.ml **)
(*       module pour faire afficher des peintures        *)
(*********************************************************)
#open "graphics" ;;
(* test *)
(* Fait apparaitre la fen?tre graphique d?coup?e en bandes
   verticales color?es. Les couleurs sont donn?es par une
   liste de triplets (les composantes RVB). L'appui sur 
   touche quelconque fait quitter la fen?tre graphique    *)

let bandes lc = let n = list_length lc
                and dx = ref 0 and rx = ref 0
                and c = ref (255,255,255)
                and x = ref 0 and x2 = ref 0
                and h = ref 0
                and l = ref lc
          in if n>0 then
          begin
          open_graph "";
          dx := size_x()/n; rx := size_x() mod n;
          h := size_y();
          x2 := 0;
          for i=1 to n
          do let (r,g,b) = hd(!l) 
             in set_color (rgb r g b) ;
             l := tl(!l);
             x := !x2; x2 := !x + !dx;
             if !rx>0 then begin
                           rx := !rx-1; x2 := !x2+1
                           end;
             fill_rect !x 0 (!x2- !x+1) !h
             done;
       let c=read_key () in close_graph ()
       end ;;





