Yutils = require "Yutils"

function math.distance(...)
	local sum=0
	for k,v in ipairs{...}
		do
		sum=sum+v^2
	end
	return math.sqrt(sum)
end

function permu(m,n)--number，排列
	if (m<0 or n<0)
		then
		_G.error("m and n must be greater than 0")
	else
	local out=1
	if n>m
		then
		_G.error("n must be smaller than m")
	else
		for i=m,m-n+1,-1
			do
			out=out*i
		end
	end
	return out
end
end

function combin(m,n)--number，组合
	if m==n
		then
		return 1
	else
		return permu(m,n)/permu(n,n)
	end
end

function loop_n(tbl)--table,table,number，loop分组(支持非均分)
	local temp_maxj={}
	temp_maxj[0]=0
	loop_maxj={fx1={},fx2}
	local loop_j={fx1={},fx2={}}
	loop_j.fx2[0]=1
	for i=1,#tbl
		do 
		max_loop=(i==1 and tbl[i] or max_loop+tbl[i])
		temp_maxj[i]=temp_maxj[i-1]+tbl[i]
	end
	for i=1,max_loop
		do
		  if i>temp_maxj[loop_j.fx2[i-1]]
		  then 
			  loop_j.fx2[i]=loop_j.fx2[i-1]+1
		  else 
			  loop_j.fx2[i]=loop_j.fx2[i-1]
		  end
			 loop_j.fx1[i]=i-temp_maxj[loop_j.fx2[i]-1]
			 loop_maxj.fx1[i]=tbl[loop_j.fx2[i]]
	end
	    loop_maxj.fx2=loop_j.fx2[#loop_j.fx2]
	return loop_j,loop_maxj,max_loop
end

function frame_fad(alpha,duration,dur1,dur2,i,max,ms)--string，透明度逐帧渐变
	local tag=""
	if i<=math.ceil(dur1/ms)
		then 
		tag=_G.ass_alpha(255-(255-alpha)*i/math.ceil(dur1/ms))
	elseif i>=max-math.ceil(dur2/ms)
		then 
		tag=_G.ass_alpha(alpha+(255-alpha)*(i-max+math.ceil(dur2/ms))/(max-max+math.ceil(dur2/ms)))
	else
		tag=_G.ass_alpha(alpha)
	end
	return tag
end

function num(origin,count)--number，浮点数处理
   if origin==0
   then
   num_out=0
   else
   	if count
   		then
   local d,float=math.modf(origin)
   local float=math.abs(float*(10^count))
   num_out=d+math.abs(origin)/origin*(math.floor(float+0.5)/(10^count))
else
	num_out=math.floor(origin+0.5)
end
   end
   return num_out
end

function table.mix(tbl)--table，打乱集合元素
	for i=1,#tbl
		do
		local var=math.random(#tbl)
		tbl[i],tbl[var]=tbl[var],tbl[i]
	end
	return tbl
end

function table.add(tbl1,tbl2)--table，合并集合元素
	for i=1,#tbl2
		do
		tbl1[#tbl1+1]=tbl2[i]
	end
	return tbl1
end

function table.num_add(tbl1,tbl2)--table，将两个数组的元素相加并返回新数组
	local new_tbl={}
	if (_G.type(tbl1)~="table" or _G.type(tbl1)~="table")
		then
		_G.error("tbl1 and tbl2 must be table value")
	end
	if (#tbl1>#tbl2)
		then
		_G.error("tbl1长度大于tbl2长度")
		elseif(#tbl1<#tbl2)
			then
			_G.error("tbl2长度大于tbl1长度"..string.format("：差为%d",#tbl2-#tbl1))
	else
	for i=1,#tbl1
		do
		new_tbl[#new_tbl+1]=tbl1[i]+tbl2[i]
	end
	return new_tbl
end
end

function table.num_multi(tbl,num)--table，将数组的元素数乘num并返回新数组
	local new_tbl={}
	for i=1,#tbl
		do
		new_tbl[i]=tbl[i]*num
	end
	return new_tbl
end

function ergodic_rand(min,max,integral)--table，产生遍历随机数(离散值)
	local rand={}
	if (not integral)
		then for i=1,max-min+1
		do rand[#rand+1]=min+i-1
	end
	for i=1,#rand
		do
		local var=math.random(#rand)
		rand[i],rand[var]=rand[var],rand[i]
	end
else
	for i=1,max-min+1
		do rand[#rand+1]=min+i-1+math.random()
	end
	for i=1,#rand
		do
		local var=math.random(#rand)
		rand[i],rand[var]=rand[var],rand[i]
	end
end
return rand
end

function point_cut(str,mod_str)--table，按模式截取字符串(不包含模式字符串本身)
	local k,cut=0,{} 
	local len=_G.unicode.len(str)
	local out={}
	for s in _G.unicode.chars(str) 
		do 
		k=k+1
		if s==mod_str
		then 
		cut[#cut+1]=k
		end
		end
	if k==0 then out[1]=str
	else 
		for i=1,#cut
		do 
		out[#out+1]=string.sub(str,cut[i]+1,i==#cut and len or cut[i+1]-1)
		end
	end
		return out
end

function get_pos(shape)--table，获取绘图坐标值
	local getpos={x={},y={}}
	if shape==""
		then
		getpos.x[1]=0
		getpos.y[1]=0
	else
	local num={}
	local out=point_cut(shape," ")
	for i=1,#out
	do 
	num[#num+1]=_G.tonumber(out[i])
end
for i=1,#num/2
	do 
	getpos.x[#getpos.x+1],getpos.y[#getpos.y+1]=num[2*i-1],num[2*i]
end
end
return getpos
end

function points_inside_shape(ass_shape,max_points)--table，获取绘图内部点坐标
	local pos=_G.table.mix(_G.Yutils.shape.to_pixels(ass_shape))
	local outpos={}
	for i=1,math.min(#pos,max_points)
		do
		outpos[i]={x=pos[i].x,y=pos[i].y}
	end
	return outpos
end

function points_in_shape(ass_shape,max_points)--table，获取若干绘图命令点坐标
	local vec={}
	for s in string.gmatch(ass_shape,"[a-z][^a-z]+")
		do
		vec[#vec+1]=s
	end
	vec=_G.table.mix(vec)
	local outpos={}
	for i=1,math.min(max_points,#vec)
		do
		outpos[i]={x=get_pos(vec[i]).x[#get_pos(vec[i]).x],y=get_pos(vec[i]).y[#get_pos(vec[i]).y]}
	end
	return outpos
end

function poly(num_tbl1,num_tbl2)--table，多项式积各项系数计算
	num_out={}
	for i=1,#num_tbl1+#num_tbl2-1
		do
		num_out[#num_out+1]=0
	end 
	for i=1,#num_tbl1 
		do 
			for p=1,#num_tbl2 
				do 
					num_out[i+p-1]=num_out[i+p-1]+num_tbl1[i]*num_tbl2[p]
			end
	end
	return num_out
end

function poly_pow(c_tbl,n_tbl,n)--table,table，二项式展开系数计算
	if #c_tbl~=#n_tbl
		then
		_G.error("多项式项数与系数数量不对应")
	else
	local new_n={}
	local new_c=c_tbl
	if (n==0)
		then
		new_c,new_n={1},{0}
	elseif (n<0)
		then
		_G.error("次数不能为负数")
		elseif(num(n)~=n)
			then
			_G.error("次数不能为浮点数")
			else
	for i=1,n-1
		do
		new_c=poly(new_c,c_tbl)
	end
	for i=1,#new_c
		do
		new_n[i]=#new_c-i
	end
end
	return new_c,new_n
end
end

function bezier_coe_count(tbl)--table,table，贝塞尔曲线参数方程多项式系数计算
	local n=#tbl
	local c_tbl={}
	local n_tbl={}
	local c_tbl_single={}
	for i=0,n-1
		do
		c_tbl[i+1]=0
		c_tbl_single[i+1]=_G.table.num_multi(poly(poly_pow({1,0},{1,0},i),poly_pow({-1,1},{1,0},n-i-1)),tbl[i+1]*combin(n-1,i))
		n_tbl[i+1]=n-1-i
	end
	for p=1,#c_tbl_single
		do
		c_tbl=_G.table.num_add(c_tbl,c_tbl_single[p])
	end
	return c_tbl,n_tbl
end

function shape_get1pos(ass_shape)--number,number，获取绘图第一个命令点坐标
	return get_pos(ass_shape).x[1],get_pos(ass_shape).y[1]
end

function shape_single_point_detect(ass_shape)--string，检测有无单独命令点并删除
	local part={}
	for s in string.gmatch(ass_shape,"[m] [^m]+")
		do
		local pos=get_pos(s)
		if #pos.x~=1
			then
		part[#part+1]=s
	end
	end
	return table.concat(part)
end

function three_order_func_count(tbl,t)--number，计算在t处的三次函数值
	local sum=0
	for i=0,3
		do
		sum=sum+tbl[i+1]*math.pow(t,3-i)
	end
	return sum
end

gauss_x={-0.9931285991850940,
		-0.9639719272779130,
		-0.9122344282513260,
		-0.8391169718222180,
		-0.7463319064601500,
		-0.6360536807265150,
		-0.5108670019508270,
		-0.3737060887154190,
		-0.2277858511416450,
		-0.0765265211334973,
		0.0765265211334973,
		0.2277858511416450,
		0.3737060887154190,
		0.5108670019508270,
		0.6360536807265150,
		0.7463319064601500,
		0.8391169718222180,
		0.9122344282513260,
		0.9639719272779130,
		0.9931285991850940}

gauss_w={0.0176140071391521,
		0.0406014298003689,
		0.0626720483341090,
		0.0832767415767047,
		0.1019301198172400,
		0.1181945319615180,
		0.1316886384491760,
		0.1420961093183820,
		0.1491729864726030,
		0.1527533871307250,
		0.1527533871307250,
		0.1491729864726030,
		0.1420961093183820,
		0.1316886384491760,
		0.1181945319615180,
		0.1019301198172400,
		0.0832767415767047,
		0.0626720483341090,
		0.0406014298003689,
		0.0176140071391521}

function bezier_position(x_tbl,y_tbl,t)--n次贝赛尔曲线在t处的坐标值
	local function single_pos(tbl,t)
		local pos=0
		local n=#tbl-1
		for i=1,#tbl
			do
			pos=pos+tbl[i]*combin(n,i-1)*(t^(i-1))*((1-t)^(n-i+1))
		end
		return pos
	end
	return single_pos(x_tbl,t),single_pos(y_tbl,t)
end

function bezier_n_derivatives(tbl,t)
	local n=#tbl-1
	local der=0
	for i=1,n-1
		do
		der=der+combin(n,i)*tbl[i+1]*((1-t)^(n-i-1))*(t^(i-1))*(i-n*t)
	end
	der=der-tbl[1]*n*((1-t)^(n-1))+tbl[#tbl]*n*((t)^(n-1))
	return der
end

function bezier_n_speed(x_tbl,y_tbl,t)
	local dx,dy=bezier_n_derivatives(x_tbl,t),bezier_n_derivatives(y_tbl,t)
	return math.distance(dx,dy),dx,dy
end

function bezier_n_length(x_tbl,y_tbl,t)
	local len=0
	for i=1,#gauss_x
		do
		local ct=gauss_x[i]*t*0.5+t*0.5
		local v=bezier_n_speed(x_tbl,y_tbl,ct)
		len=len+gauss_w[i]*t*v/2
	end
	return len
end

function bezier_n_t_at_percent(x_tbl,y_tbl,pct)--三次贝塞尔曲线在百分之pct长度处的参数值
	local length=bezier_n_length(x_tbl,y_tbl,1)
	local s=length*pct/100
	local a=pct/100
	local k=0
	if a==0 or a==1
		then
		return a
	else
		while(true)
			do
			k=k+1
			b=a
			a=a-(bezier_n_length(x_tbl,y_tbl,a)-s)/bezier_n_speed(x_tbl,y_tbl,a)
			if (math.abs(a-b)<0.00001 or k>=5000)--设定精度并防止死循环
				then
				break
			end
		end
		return a
	end
end

function bezier_n_uniform_speed(x_tbl,y_tbl,particle_width)
	local length=bezier_n_length(x_tbl,y_tbl,1)
	local max_n=math.ceil(length/particle_width)+1
	local pos={}
	for i=1,max_n
		do
		local t_r=bezier_n_t_at_percent(x_tbl,y_tbl,(i-1)*100/(max_n-1))
		local v,vx,vy=bezier_n_speed(x_tbl,y_tbl,t_r)
		local v1,vx1,vy1=bezier_n_speed(x_tbl,y_tbl,(i-1)/(max_n-1))
		local degree=math.deg(_G.comp_arg({vx,vy}))
		local degree1=math.deg(_G.comp_arg({vx1,vy1}))
		pos[#pos+1]={}
		pos[#pos].x,pos[#pos].y=bezier_position(x_tbl,y_tbl,t_r)
		pos[#pos].x1,pos[#pos].y1=bezier_position(x_tbl,y_tbl,(i-1)/(max_n-1))
		pos[#pos].deg=-degree
		pos[#pos].deg1=-degree1
	end
	return pos
end

function get_command_length(vector1,vector2)
	local len=0
	local pos1,pos2=get_pos(vector1),get_pos(vector2)
	local s_x,s_y=pos1.x[#pos1.x],pos1.y[#pos1.y]
	if #pos2.x==1
		then
		len=math.sqrt(math.pow(s_x-pos2.x[1],2)+math.pow(s_y-pos2.y[1],2))
	else
		local x0,y0=pos1.x[#pos1.x],pos1.y[#pos1.y]
		table.insert(pos2.x,1,x0)
		table.insert(pos2.y,1,y0)
		len=len+bezier_n_length(pos2.x,pos2.y,1)
	end
	return len
end

function shape_length_single(ass_shape)
	local vector={}
	for s in string.gmatch(ass_shape,"[a-z] [^a-z]+")
		do
		vector[#vector+1]=s
	end
	for i=1,#vector
		do
		length=(i==1 and 0 or length)+get_command_length(vector[i],vector[math.fmod(i,#vector)+1])
	end
	return length
end

function shape_len(ass_shape)
	local sum=0
	for s in string.gmatch(ass_shape,"[m] [^m]+")
		do
		sum=sum+shape_length_single(s)
	end
	return sum
end

function get_command_point(vector1,vector2,t)
	local pos1,pos2=get_pos(vector1),get_pos(vector2)
	local s_x,s_y=pos1.x[#pos1.x],pos1.y[#pos1.y]
	if #pos2.x==1
		then
		local e_x,e_y=pos2.x[1],pos2.y[1]
		i_x,i_y=(e_x-s_x)*t+s_x,(e_y-s_y)*t+s_y
		return num(i_x,1),num(i_y,1)
	else
		local c_x_tbl={s_x,pos2.x[1],pos2.x[2],pos2.x[3]}
		local c_y_tbl={s_y,pos2.y[1],pos2.y[2],pos2.y[3]}
		local tr=bezier_n_t_at_percent(c_x_tbl,c_y_tbl,t*100)
		local i_x,i_y=bezier_position(c_x_tbl,c_y_tbl,tr)
		return num(i_x,1),num(i_y,1)
	end
end

function shape_get_point_single(ass_shape,particle_width,mode)
	local vec={}
	local p_pos={}
	local p_w=particle_width
	for s in string.gmatch(ass_shape,"[a-z] [^a-z]+")
		do
		vec[#vec+1]=s
	end
	if (mode=="close" or mode=="c")
		then
		mvec=#vec
	elseif (mode=="open" or mode=="o")
		then
		mvec=#vec-1
	elseif (not mode)
		then
		mvec=#vec
	else
		error("mode only supports \"close(c)\" or \"open(o)\"",2)
	end
	for i=1,mvec
		do
		local len=get_command_length(vec[i],vec[math.fmod(i,#vec)+1])
		local max_n=math.ceil(len/p_w)
		for p=1,max_n
			do
			p_pos[#p_pos+1]={x,y}
			p_pos[#p_pos].x,p_pos[#p_pos].y=get_command_point(vec[i],vec[math.fmod(i,#vec)+1],p/max_n)
		end
	end
	return p_pos
end

function shape_get_point(ass_shape,particle_width,mode)
	local part={}
	local pos={}
	for s in string.gmatch(ass_shape,"[m] [^m]+")
		do
		part[#part+1]=s
	end
	for i=1,#part
		do
		pos=table.add(pos,shape_get_point_single(part[i],particle_width,mode))
	end
	return pos
end

function vector_intergration(vector)--string，绘图命令标准化
	local pos=get_pos(vector)
	local vec=""
	if string.match(vector,"l")
		then
		for i=1,#pos.x
			do
			vec=vec.."l "..pos.x[i].." "..pos.y[i].." "
		end
		elseif string.match(vector,"b")
			then
				for i=1,#pos.x
					do
					vec=vec..(i%3==1 and "b " or "")..pos.x[i].." "..pos.y[i].." "
				end
			else
				vec=vector
			end
	return vec
end

function shape_intergration(ass_shape)--string，图形标准化
	local vector={}
	local ass_shape=_G.Yutils.shape.filter(string.gsub(string.gsub(ass_shape,"c",""),"s",""),function (x,y) return num(x),num(y) end)
	for s in string.gmatch(ass_shape,"[a-z][^a-z]+")
		do
		vector[#vector+1]=vector_intergration(s)
	end
	return _G.table.concat(vector)
end

function vector_split(vector1,vector2,line_width)--将单个贝塞尔曲线绘图命令按长度(line_width)分割为直线段
	local l_w=line_width
	local vec2=vector2
	local pos1,pos2=get_pos(vector1),get_pos(vector2)
	local sx,sy=pos1.x[#pos1.x],pos1.y[#pos1.y]
	if #pos2.x==1
		then
		return vector2
	else
		local len=get_command_length(vector1,vector2)
		local max_p=math.ceil(len/l_w)
		for i=1,max_p
			do
			local i_x,i_y=get_command_para_pos(vector1,vec2,i/max_p)
			vector2=(i==1 and "" or vector2).."l ".._G.num(i_x).." ".._G.num(i_y).." "
		end
		return vector2
	end
end

function shape_flat_single(ass_shape)
	local vec={}
	for s in string.gmatch(ass_shape,"[a-z] [^a-z]+")
		do
		vec[#vec+1]=s
	end
	for i=2,#vec
		do
		vec[i]=vector_split(vec[i-1],vec[i],1)
	end
	return _G.table.concat(vec)
end

function shape_flat(ass_shape)
	local part={}
	for s in string.gmatch(ass_shape,"[m] [^m]+")
		do
		part[#part+1]=_G.shape_flat_single(s)
	end
	return _G.table.concat(part)
end

function comp_arg(tbl)--number，复数主幅角
	if tbl[1]>0
	then
	return math.atan(tbl[2]/tbl[1]) 
	elseif (tbl[1]<0 and tbl[2]~=0)
		then
		return math.pi*math.abs(tbl[2])/tbl[2]+math.atan(tbl[2]/tbl[1])
		elseif (tbl[1]<0 and tbl[2]==0)
			then 
			return math.pi
			elseif (tbl[1]==0 and tbl[2]~=0)
				then
				return math.pi*(math.abs(tbl[2])/tbl[2])/2
				elseif (tbl[1]==0 and tbl[2]==0)
					then
					return false
		end
	end