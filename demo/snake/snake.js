var Snake =
 (function () {
   var TO=TypedObject;
   var self = {
compile: function () { return fetch('snake.wasm').then(WebAssembly.compileStreaming) },

 desc:
 {
'Object':0,
'Board':12,
'Tile':44,
'Empty':76,
'Wall':112,
'Body':148,
'Food':184,
},
 types:
 {'Object':new TO.StructType({_desc_:TO.int32}),
'Board':new TO.StructType({_desc_:TO.int32,'height':TO.int32,'width':TO.int32,'tiles':TO.Object}),
'Tile':new TO.StructType({_desc_:TO.int32,'element':TO.Object}),
'Empty':new TO.StructType({_desc_:TO.int32,'element':TO.Object}),
'Wall':new TO.StructType({_desc_:TO.int32,'element':TO.Object,'rendering':TO.string}),
'Body':new TO.StructType({_desc_:TO.int32,'element':TO.Object,'younger_y':TO.int32,'younger_x':TO.int32}),
'Food':new TO.StructType({_desc_:TO.int32,'element':TO.Object}),
},
 strings:
 [
"-",
"|",
"+",
" ",
"#",
"*",
"*** CRASHED ***",
"Paused",
"Can't restart yet",
"Going",
],
 buffer:[],
 lib:
 {
'_new_vector_Tile':
function (n,init) {
  let a=new Array(n);
  for (let i=0; i < n; i++)
    a[i]=init;
  a._tag=0;
  return a;
},
'_new_Board':function (height,width,tiles) { return new self.types.Board({_desc_:self.desc.Board,height,width,tiles}) },
'_string_literal':function (n) { return self.strings[n] },
'_new_Wall':function (element,rendering) { return new self.types.Wall({_desc_:self.desc.Wall,element,rendering}) },
'_upcast_class_to_Tile':function (p) { return p },
'_new_Empty':function (element) { return new self.types.Empty({_desc_:self.desc.Empty,element}) },
'_get_Board_tiles':function (p) { return p.tiles },
'_get_Board_width':function (p) { return p.width },
'_vector_ref_Tile':
function (p,i) {
  if ((i >>> 0) >= p.length)
    throw new RangeError('Out of range: ' + i + ' for ' + p.length);
  return p[i];
},
'_vector_set_Tile':
function (p,i,v) {
  if ((i >>> 0) >= p.length)
    throw new RangeError('Out of range: ' + i + ' for ' + p.length);
  p[i] = v;
},
'_get_Tile_element':function (p) { return p.element },
'_set_Tile_element':function (p, v) { p.element = v },
'_desc_':function (p) { return p._desc_ },
'_get_Wall_element':function (p) { return p.element },
'_get_Wall_rendering':function (p) { return p.rendering },
'_get_Board_height':function (p) { return p.height },
'_upcast_class_to_anyref':function (p) { return p },
'_new_Body':function (element,younger_y,younger_x) { return new self.types.Body({_desc_:self.desc.Body,element,younger_y,younger_x}) },
'_new_Food':function (element) { return new self.types.Food({_desc_:self.desc.Food,element}) },
'_set_Body_younger_y':function (p, v) { p.younger_y = v },
'_set_Body_younger_x':function (p, v) { p.younger_x = v },
'_get_Body_younger_y':function (p) { return p.younger_y },
'_get_Body_younger_x':function (p) { return p.younger_x },
}
 };
 return self;
 })();

 {
   let board = null;
   let row   = null
   let clock = null;
   let snake = null;
   let start = function(module) {
     snake = new WebAssembly.Instance(
     module,
     { lib   : Snake.lib,
       debug : { prs : console.log, pri: console.log },
       Math  : Math,
       hacks : { 'stash-board'   : function (b) { board = b; return board },
                 'unstash-board' : function () { return board }
               },
       dom   : { setText(element, text) { element.textContent = text },
                 newRow() { row = document.getElementById('grid').appendChild(document.createElement('div')) },
                 newTile() { return row.appendChild(document.createElement('span')) },
                 startClock() { interval = setInterval(snake.ontick, 20) },
                 stopClock() { clearInterval(interval) },
                 setState(s) { document.getElementById('state').textContent = s },
                 focus() { /* what to do? */ }
               }
     }).exports;
     snake.setup(24, 80)
   }
   window.addEventListener('load', () => Snake.compile().then(start, function(err){ throw err }));
   window.addEventListener('keypress', (ev) => snake.onkey(ev.charCode));
 }

