SpreadView : ValuesView {
	var <innerRadiusRatio, <outerRadiusRatio, boarderPx;
	var <bnds, <cen, <maxRadius, <innerRadius, <outerRadius, <wedgeWidth; // set in drawFunc, for access by drawing layers
	var <handlePnts, <handleThetas, <valTheta;
	var clickRangePx;
	var <direction, <rangeCenterOffset;
	var <dirFlag; 				// cw=1, ccw=-1
	var <rangeStartAngle, <rangeSweepLength, <prRangeSweepLength, <prRangeStartAngle;

	// drawing layers. Add getters to get/set individual properties by '.p'
	var <range, <sprd, <handle, <curvalue, <label;
	/*
	spreadSpec: spec describing spread in radians, other specs will be inferred from this
	initVals: order of values in arrays - [value, center, spread, lo, hi], lo and hi can be nil to autofill from center/spread
	rangeCenterOffset: offset of "0" position from default (0 o'clock), degrees
	innerRadiusRatio=0: ratio of inner space
	outerRadiusRatio=1; ratio of outer edge of spread wedge
	*/

	*new {
		|parent, bounds, maxSpread, initVals, rangeCenterOffset=0, innerRadiusRatio=0, outerRadiusRatio=1|
		^super.new(parent, bounds, nil, initVals).init(rangeCenterOffset, innerRadiusRatio, outerRadiusRatio, maxSpread, initVals);
	}


	init {
		|argRangeCenterOffset, argInnerRadiusRatio, argOuterRadiusRatio, maxSpread, initVals|
		// REQUIRED: in subclass init, initialize drawing layers
		// initialize layer classes and save them to vars
		#range, sprd, handle, curvalue, label = [
			SprdRangeLayer, SprdSpreadLayer, SprdHandleLayer, SprdCurvalueLayer, SprdLabelLayer
		].collect({
			|class|
			class.new(this, class.properties)
		});
		// convenience variable to access a list of the layers
		layers = [range, sprd, handle, curvalue, label];

		rangeCenterOffset = argRangeCenterOffset;

		direction = \cw;
		dirFlag = 1;
		boarderPx = 1;

		this.initSpecsAndVals(maxSpread, initVals);

		this.rangeStartAngle = maxSpread.half.neg;		// reference 0 is UP
		this.rangeSweepLength = maxSpread;

		this.innerRadiusRatio_(argInnerRadiusRatio); // set innerRadiusRatio with setter to check sweepLength condition
		this.outerRadiusRatio_(argOuterRadiusRatio);

		// intialize pixel unit variables
		maxRadius = this.bounds.width/2;
		outerRadius = maxRadius*outerRadiusRatio;
		innerRadius = maxRadius*innerRadiusRatio;
		wedgeWidth = outerRadius-innerRadius;

		clickRangePx = if (handle.p.radius<1){handle.p.radius*outerRadius}{handle.p.radius};

		this.defineMouseActions;
		this.direction_(direction);  // this initializes prStarAngle and prSweepLength
	}

	// initialize [value, center, spread, lo, hi] specs and values
	initSpecsAndVals { |maxSpread, initVals|
		var lo, hi, valSpec, cenSpec, sprdSpec, loSpec, hiSpec, spcs;
		var v, c, s, l, h;
		var initError;

		// re-initialize specs
		lo = maxSpread.half.neg;
		hi = maxSpread.half;
		valSpec = [lo, hi].asSpec;
		cenSpec = [lo, hi].asSpec;
		sprdSpec = [0, maxSpread].asSpec;
		loSpec = [lo, hi].asSpec;
		hiSpec = [lo, hi].asSpec;
		// reset specs instance variable to be the array of specs
		spcs = [valSpec, cenSpec, sprdSpec, loSpec, hiSpec];

		spcs.do{|spec, i| this.specAt_(i, spec, false)};

		// re-initialize values
		// NOTE: center/spread vals take precedence over lo/hi
		#v, c, s, l, h = initVals;

		initError = {
			format(
				"Provide intial values for either center/spread or lo/hi. Provided: center % spread % lo % hi %\n",
				c, s, l, h
			).throw
		};

		if (c.isNil or: s.isNil) {
			if (l.isNil or: h.isNil) {initError.()} {
				// make sure both are specified
				if (l.notNil and: h.notNil) {initError.()};
				// this will update center and spread
				this.lo_(l, false);
				this.hi_(h, true);
			}
		} {
			// make sure both are specified
			if (c.notNil and: s.notNil) {
				this.center_(c, false);
				this.spread_(s, true);
			} {initError.()};
		};
		// re-init values by spread and center
		this.curValue_(v, false);
	}

	curValue_ { |deg, broadcast=true|
		this.valueAt_(0, deg, broadcast)
	}
	curValueDoAction_ { |deg|
		this.valueAtDoAction_(0, deg)
	}

	prOkToSet { |newcen, newspread|
		var boundDists, okToSet = true;
		var h_sprd, newLo, newHi;

		h_sprd = newspread.half;
		newLo = newcen-h_sprd;
		newHi = newcen+h_sprd;
		// distance from bound, vals should be positive if in bounds
		boundDists = [newLo - specs[0].minval,	specs[0].maxval - newHi];

		boundDists.do{ |dist|
			if (dist.isNegative) {
				okToSet = false;
				// "Hit bounds.".warn;
			}
		};
		^okToSet
	}
	// the following setters are for setting each parameter
	// anchored by it's opposite and updating the others
	// i.e. center update: spread anchored, lo/hi auto-update

	center_ { |deg, broadcast=true|
		if ( this.prOkToSet(deg, this.spread) ) {
			this.prUpdateLoHi(deg, this.spread, false);
			this.valueAt_(1, deg, broadcast);
		};
	}

	centerDoAction_ { |deg|
		if ( this.prOkToSet(deg, this.spread) ) {
			this.prUpdateLoHi(deg, this.spread, false);
			this.valueAtDoAction_(1, deg);
		};
	}

	spread_ { |deg, broadcast=true|
		if ( this.prOkToSet(this.center, deg) ) {
			this.prUpdateLoHi(this.center, deg, false);
			this.valueAt_(2, deg, broadcast)
		}
	}

	spreadDoAction_ { |deg|
		if ( this.prOkToSet(this.center, deg) ) {
			this.prUpdateLoHi(this.center, deg, false);
			this.valueAtDoAction_(2, deg);
		}
	}


	// if setting lo or hi explicitly, the opposite bound remains
	// and center/spread is updated
	lo_ { |deg, broadcast=true|
		this.prUpdateCenSprd(deg, this.hi, false);
		this.valueAt_(3, deg, broadcast)
	}
	loDoAction_ { |deg|
		this.prUpdateCenSprd(deg, this.hi, false);
		this.valueAtDoAction_(3, deg)
	}

	hi_ { |deg, broadcast=true|
		this.prUpdateCenSprd(this.lo, deg, false);
		this.valueAt_(4, deg, broadcast)
	}
	hiDoAction_ { |deg|
		this.prUpdateCenSprd(this.lo, deg, false);
		this.valueAtDoAction_(4, deg)
	}

	// update lo and hi by center/spread
	prUpdateLoHi { |center, spread, broadcast|
		var h_sprd;
		h_sprd = spread.half;
		this.valueAt_(3, center - h_sprd, broadcast); // lo
		this.valueAt_(4, center + h_sprd, broadcast); // hi
	}
	// update center/spread by lo and hi
	prUpdateCenSprd { |lo, hi, broadcast|
		var sprd;
		sprd = hi - lo;
		// don't use convenience methods or else inf loop
		this.valueAt_(1, sprd / 2, broadcast);  // cen
		this.valueAt_(2, sprd, broadcast);			// sprd
	}

	// state getters
	curValue { ^this.valueAt(0) }
	center { ^this.valueAt(1) }
	spread { ^this.valueAt(2) }
	lo { ^this.valueAt(3) }
	hi { ^this.valueAt(4) }

	drawFunc {
		^{|v|
			// "global" instance vars, accessed by ValueViewLayers
			bnds = v.bounds;
			cen  = bnds.center;
			maxRadius = min(cen.x, cen.y) - boarderPx;
			outerRadius = maxRadius * outerRadiusRatio;
			innerRadius = maxRadius * innerRadiusRatio;
			wedgeWidth = outerRadius - innerRadius;

			clickRangePx = if (handle.p.radius<1){handle.p.radius*outerRadius}{handle.p.radius};

			this.calcHandlePnts;

			this.drawInThisOrder;
		};
	}

	calcHandlePnts {
		var bndrho, cenrho;
		// lo, center, hi
		handleThetas = [inputs[3], inputs[1], inputs[4]].collect{ |rot|
			var theta, rho;
			theta = prRangeStartAngle + (rot * prRangeSweepLength);
		};
		bndrho = innerRadius + (wedgeWidth * handle.p.anchorBnd);
		cenrho = innerRadius + (wedgeWidth * handle.p.anchorCen);
		handlePnts = [bndrho, cenrho, bndrho].collect{|rho, i|
			Polar(rho, handleThetas[i]).asPoint + cen;
		};

		// valPoint's reference is the outer edge
		valTheta = prRangeStartAngle + (inputs[0] * prRangeSweepLength);
	}

	drawInThisOrder {
		if (range.p.show) {range.fill};
		if (sprd.p.show) {sprd.fill};
		if (handle.p.show) {
			if (handle.p.fill) {handle.fill};
			if (handle.p.stroke) {handle.stroke};
		};
		if (label.p.show) {label.fill};
		if (curvalue.p.show) {curvalue.stroke};
	}

	defineMouseActions {
		var clicked, adjustLo, adjustCen, adjustHi, mDownPnt;
		clicked = adjustLo = adjustCen = adjustHi = false;

		// assign action variables: down/move
		mouseDownAction = {
			|v, x, y|
			mDownPnt = x@y;

			block { |break|
				handlePnts.do{|hpnt, i|
					if (hpnt.dist(mDownPnt) < clickRangePx) {
						clicked = true;
						switch(i,
							0, {adjustLo = true},
							1, {adjustCen = true},
							2, {adjustHi = true},
						);
						break.()
					};
				}
			}
		};

		mouseMoveAction  = {
			|v, x, y|
			var pos, theta, posDeg;
			if (clicked) {
				pos = ((x@y) - cen);
				theta = atan2(pos.y,pos.x); // radian position, relative 0 at 3 o'clock
				// where in the range the mouse is
				posDeg = specs[0].map(
					(theta - prRangeStartAngle).wrap(0, 2pi) / prRangeSweepLength
				);

				case
				{adjustLo} { // changes spread
					// prevent snapping around prRangeStartAngle
					if (posDeg <= this.hi){
						this.spreadDoAction_(this.center - posDeg * 2);
					}
				}
				{adjustHi} { // changes spread
					if (posDeg >= this.lo){
						this.spreadDoAction_(posDeg - this.center * 2);
					}
				}
				{adjustCen} {
					this.centerDoAction_(posDeg);
				};
			};
		};

		mouseUpAction = {
			|v, x, y|
			clicked = adjustLo = adjustCen = adjustHi = false;
		}
	}

	direction_ {|dir=\cw|
		direction = dir;
		dirFlag = switch (direction, \cw, {1}, \ccw, {-1});
		this.rangeStartAngle_(rangeStartAngle);
		this.rangeSweepLength_(rangeSweepLength);		// updates prSweepLength
		this.refresh;
	}

	rangeStartAngle_ {|deg=0|
		rangeStartAngle = deg;
		prRangeStartAngle = -0.5pi + (rangeCenterOffset + rangeStartAngle).degrad;		// start angle always relative to 0 is up, cw
	}

	rangeSweepLength_ {|deg=360|
		rangeSweepLength = deg;
		prRangeSweepLength = rangeSweepLength.degrad * dirFlag;
		this.innerRadiusRatio_(innerRadiusRatio); // update innerRadiusRatio in case this was set to 0
	}

	innerRadiusRatio_ {|ratio|
		innerRadiusRatio = if (ratio == 0) {1e-5} {ratio};
		this.refresh
	}

	outerRadiusRatio_ {|ratio|
		outerRadiusRatio = ratio;
		this.refresh
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
		this.fillFromLength(view.prRangeStartAngle, view.prRangeSweepLength)
	}
}

SprdSpreadLayer : RotaryArcWedgeLayer {
	*properties {
		^(
			show:					true,					// show this layer or not
			style:				\wedge,				// \wedge or \arc: annularWedge or arc
			// note if \arc, the width follows .width, not strokeWidth
			width:				1,						// width of either annularWedge or arc; relative to wedgeWidth
			radius:				1,						// outer edge of the wedge or arc; relative to maxRadius
			fill:		 			true,					// if annularWedge
			fillColor:		Color.green.alpha_(0.3),
			stroke:				true,
			strokeColor:	Color.gray,
			strokeType:		\around, 			// if style: \wedge; \inside, \outside, or \around
			strokeWidth:	1, 						// if style: \wedge, if < 1, assumed to be a normalized value and changes with view size, else treated as a pixel value
			capStyle:			\round,				// if style: \arc
			joinStyle:	 	0,						// if style: \wedge; 0=flat
		)
	}

	fill {
		this.fillFromLength(
			view.prRangeStartAngle + (view.inputs[3] * view.prRangeSweepLength),
			view.inputs[2] * view.prRangeSweepLength
		)
	}
}

//
SprdHandleLayer : ValueViewLayer {
	*properties {
		^(
			show:					true,					// show this layer or not
			anchorBnd:		0.5,					// relative to outerRadius
			anchorCen:		1,						// relative to outerRadius
			radius:				0.05,					// if < 1, assumed to be a normalized value and changes with view size, else treated as a pixel value
			fill:		 			true,
			fillColor:		Color.blue.alpha_(0.3),
			stroke:				true,
			strokeColor:	Color.black,
			strokeWidth:	0.15, 						// ratio of radius
		)
	}

	fill {
		var d, rho;

		Pen.push;
		d = if (p.radius<1){p.radius*view.outerRadius}{p.radius} * 2;
		Pen.fillColor_(p.fillColor);

		view.handlePnts.do{|pnt|
			Pen.fillOval([0,0,d,d].asRect.center_(pnt))
		};
		Pen.pop;
	}

	stroke {
		var strokeWidth, d, rho;

		Pen.push;
		d = if (p.radius<1){p.radius*view.outerRadius}{p.radius} * 2;
		strokeWidth = if (p.strokeWidth<1){p.strokeWidth*d}{p.strokeWidth};
		Pen.width_(strokeWidth);
		Pen.strokeColor_(p.strokeColor);

		view.handlePnts.do{|pnt|
			Pen.strokeOval([0,0,d,d].asRect.center_(pnt))
		};
		Pen.pop;
	}
}

SprdCurvalueLayer : ValueViewLayer {
	*properties {
		^(
			show:					true,					// show this layer or not
			strokeColor: 	Color.red,
			anchor:				1,
			length:				0.5, // relative to wedgeWidth
			strokeWidth:	0.05,
		)
	}

	stroke {
		var strokeWidth, from, to;
		"drawing".postln;
		Pen.push;
		Pen.translate(view.cen.x, view.cen.y);
		Pen.strokeColor_(p.strokeColor);

		strokeWidth = if (p.strokeWidth<1){p.strokeWidth*view.outerRadius}{p.strokeWidth};
		Pen.strokeColor_(p.strokeColor);
		from = p.anchor * view.outerRadius;
		to  = from - (p.length * view.wedgeWidth);
		[from,to].postln;
		Pen.moveTo(Polar(from, view.valTheta).asPoint);
		Pen.lineTo(Polar(to, view.valTheta).asPoint);
		Pen.stroke;
		Pen.pop;
	}
}

SprdLabelLayer : ValueViewLayer {
	*properties {
		^(
			show:					true,					// show this layer or not
			showVal:			true,
			showSpread:		true,
			anchor:				1.1,					// relative to outerRadius
			bndColor: 		Color.black,
			valColor:			Color.red,
			font:					Font("Helvetica", 12),
			fontSize:			0.1,					// pointSize, or if <1 function of view size
			bndSprdCutoff:20,						// spread value at which bound labels are turned off

		)
	}

	fill {
		var rect, font, fsize, col, cen, valPnt, valsStr, xDist;
		var pnt, xoff, muteBnds=false, curValRect, drawMe;

		font = p.font;
		fsize = if(p.fontSize<1){view.maxRadius*p.fontSize}{p.fontSize};
		font.hasPointSize.if({
			font.pointSize_(fsize);
		}, {
			font.pixelSize_(fsize);
		});
		rect = "-000.0".bounds(font);
		col = p.valColor;
		cen = view.cen;
		valsStr = p.vals.round(0.1).collect({|val|val.asString});
		muteBnds = (p.vals[2] < p.bndSprdCutoff); // check spread to "mute" bound labels

		Pen.push;
		Pen.translate(cen.x, cen.y);
		Pen.strokeColor_(col);

		// cur value
		p.showVal.if{
			valPnt = Polar(p.anchor * view.outerRadius, view.valTheta);

			xoff = valPnt.theta.abs / pi;
			valPnt = valPnt.asPoint; // as a point
			rect.left = valPnt.x - (xoff*rect.width);
			if (valPnt.y > 0) {rect.top_(valPnt.y+3)} {rect.bottom_(valPnt.y+3)};

			// if (valPnt.x > cen.x) {rect.left_(valPnt.x)} {rect.right_(valPnt.x)};
			// if (valPnt.y > cen.y) {rect.top_(valPnt.y)} {rect.bottom_(valPnt.y)};
			Pen.stringCenteredIn( valsStr[0], rect, font, col);
			curValRect = rect.copy;
		};

		col = p.bndColor;
		view.handleThetas.postln;
		// lo, cen, hi
		view.handleThetas.do{|theta, i|
			drawMe = true;
			pnt = Polar(p.anchor * view.outerRadius, theta).asPoint;
			// xoff = theta.abs / pi;
			xoff = theta.fold(0,pi)/pi;
			rect.left = pnt.x - (xoff*rect.width);
			if (pnt.y > 0) {rect.top_(pnt.y+3)} {rect.bottom_(pnt.y-3)};

			// check if overlapping with curVal
			curValRect !? {
				drawMe = curValRect.contains(rect.center).not;
			};
			if (drawMe) {
				switch(i,
					0, {
						muteBnds.not.if{
							Pen.stringCenteredIn(valsStr[3], rect, font, col) // lo
						}
					},
					1, { Pen.stringCenteredIn(valsStr[1], rect, font, col) }, // cen
					2, {
						muteBnds.not.if{
							Pen.stringCenteredIn(valsStr[4], rect, font, col) // hi
						}
					},
				);
			};
		};

		// spread
		p.showSpread.if{
			Pen.stringCenteredIn(valsStr[2], rect.center_(0@0), font, col);
		};

		Pen.pop;
	}
}