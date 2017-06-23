SpreadView : ValuesView {
	var <bnds, <cen, <maxRadius, <innerRadius, <outerRadius, <wedgeWidth; // set in drawFunc, for access by drawing layers
	var <direction, <rangeCenterOffset, <orientation, <startAngle, <sweepLength;
	var <dirFlag; 				// cw=1, ccw=-1
	var <prStartAngle;

	/*
	specs, initVals: order of values in arrays - [value, center, spread, lo, hi], lo and hi can be nil to autofill from center/spread
	rangeCenterOffset: offset of "0" position from default (0 o'clock), radians
	innerRadiusRatio=0: ratio of inner space
	outerRadiusRatio=1; ratio of outer edge of spread wedge
	*/

	*new {
		|parent, bounds, specs, initVals, rangeCenterOffset=0, innerRadiusRatio=0, outerRadiusRatio=1|
		^super.new(parent, bounds, specs, initVals).init(rangeCenterOffset, innerRadiusRatio, outerRadiusRatio);
	}


	init {
		|argRangeCenterOffset, argStartAngle, argSweepLength, argInnerRadiusRatio, argOuterRadiusRatio|
		// REQUIRED: in subclass init, initialize drawing layers
		// initialize layer classes and save them to vars
		#range, spread, bounds, curvalue, label = [
			SprdRangeLayer, SprdSpreadLayer, SprdBoudndsLayer, SprdCurvalueLayer, SprdLabelLayer
		].collect({
			|class|
			class.new(this, class.properties)
		});
		// convenience variable to access a list of the layers
		layers = [range, spread, bounds, curvalue, label];

		rangeCenterOffset = argRangeCenterOffset;
		startAngle = argStartAngle;		// reference 0 is UP
		sweepLength = argSweepLength;
		direction = \cw;
		dirFlag = 1;
		orientation = \vertical;
		clickMode = \relative;			// or \absolute
		boarderPx = 1;

		this.innerRadiusRatio_(argInnerRadiusRatio); // set innerRadiusRatio with setter to check sweepLength condition
		this.outerRadiusRatio_(argOuterRadiusRatio);

		// intialize pixel unit variables
		maxRadius = this.bounds.width/2;
		outerRadius = maxRadius*outerRadiusRatio;
		innerRadius = maxRadius*innerRadiusRatio;
		wedgeWidth = outerRadius-innerRadius;

		this.defineMouseActions;
		this.direction_(direction);  // this initializes prStarAngle and prSweepLength
	}


	drawFunc {
		^{|v|
			// "global" instance vars, accessed by ValueViewLayers
			bnds = v.bounds;
			cen  = bnds.center;
			minDim = min(bnds.width, bnds.height);
			this.drawInThisOrder;
		};
	}

	drawInThisOrder {
		^{|v|
			// "global" instance vars, accessed by layers
			bnds = v.bounds;
			cen  = bnds.center;
			maxRadius = min(cen.x, cen.y) - boarderPx;
			outerRadius = maxRadius * outerRadiusRatio;
			innerRadius = maxRadius * innerRadiusRatio;
			wedgeWidth = outerRadius - innerRadius;
			this.drawInThisOrder;
		}
	}

	direction_ {|dir=\cw|
		direction = dir;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		this.startAngle_(startAngle);
		this.sweepLength_(sweepLength);		// updates prSweepLength
		this.refresh;
	}

	rangeStartAngle_ {|radians=0|
		startAngle = radians;
		prStartAngle = -0.5pi + rangeCenterOffset + startAngle;		// start angle always relative to 0 is up, cw
		this.setPrCenter;
		this.ticksAtValues_(majTickVals, minTickVals, false);		// refresh the list of maj/minTicks positions
	}

	sweepLength_ {|radians=2pi|
		sweepLength = radians;
		prSweepLength = sweepLength * dirFlag;
		this.setPrCenter;
		this.innerRadiusRatio_(innerRadiusRatio); // update innerRadiusRatio in case this was set to 0
		this.ticksAtValues_(majTickVals, minTickVals, false); // refresh the list of maj/minTicks positions
	}
}


SprdRangeLayer : RotaryArcWedgeLayer {
	// define default properties in an Event as a class method
	*properties {
		^(
			show:					true,					// show this layer or not
			style:				\wedge,				// \wedge or \arc: annularWedge or arc
																	// note if \arc, the width follows .width, not strokeWidth
			width:				1,						// width of either annularWedge or arc; relative to wedgeWidth
			radius:				1,						// outer edge of the wedge or arc; relative to maxRadius
																	// TODO: rename this?
			fill:		 			true,					// if annularWedge
			fillColor:		Color.gray.alpha_(0.3),
			stroke:				true,
			strokeColor:	Color.gray,
			strokeType:		\around, 			// if style: \wedge; \inside, \outside, or \around
			strokeWidth:	1, 						// if style: \wedge, if < 1, assumed to be a normalized value and changes with view size, else treated as a pixel value
			capStyle:			\round,				// if style: \arc
			joinStyle:	 	0,						// if style: \wedge; 0=flat
		)
	}

	fill {
		this.fillFromLength(view.prStartAngle, view.prSweepLength)
	}
}

SprdSpreadLayer
SprdBoudndsLayer
SprdCurvalueLayer
SprdLabelLayer