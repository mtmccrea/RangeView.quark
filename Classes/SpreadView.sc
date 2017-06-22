SpreadView : ValuesView {

	*new {
		|parent, bounds, specs, initVals, moveRelative=false|
		^super.new(parent, bounds, specs, initVals).init(moveRelative);
	}


}