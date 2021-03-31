<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<link rel="shortcut icon" href="/Home/sites/default/files/sky_favicon.ico" type="image/x-icon" /> 
<title>Climate Connect - Blocks</title>
<style type="text/css">

p {
  font-family:  Calibri,Verdana;
  font-size:    12pt;
}

</style>
</head>
<body bgcolor="#ffffff">
<form method="post" action="<?php echo $PHP_SELF;?>"> 
<center>
<img src="/img/logo_blocks.jpg" />
<h3> </h3>
<br />
<span style="color:#333333;">Serial No. </span>

<input type="text" name="serial1" id="serial1" style="width:26px;" value="<?php print_r($_POST["serial1"])?>" size="2" maxlength="2" onkeyup="this.value=this.value.toUpperCase();"/>-
<input type="text" name="serial2" id="serial2" style="width:21px;color:#777777;" value="05" size="2" maxlength="2" readonly="readonly" />-
<input type="text" name="serial3" id="serial3" style="width:21px;color:#777777;" value="00" size="2" maxlength="2" readonly="readonly" />-
<input type="text" name="serial4" id="serial4" style="width:72px;" value="<?php print_r($_POST["serial4"])?>" size="8" maxlength="15" onkeyup="this.value=this.value.replace(/[\D]+/gi, '');" />-
<input type="text" name="serial5" id="serial5" style="width:72px;" value="<?php print_r($_POST["serial5"])?>" size="8" maxlength="15" onkeyup="this.value=this.value.replace(/[\D]+/gi, '');" />-
<input type="text" name="serial6" id="serial6" style="width:21px;color:#777777;" value="01" size="2" maxlength="2" readonly="readonly" />-
<input type="text" name="serial7" id="serial7" style="width:21px;color:#777777;" value="01" size="2" maxlength="2" readonly="readonly" />-
<input type="text" name="serial8" id="serial8" style="width:30px;color:#777777;" value="000" size="3" maxlength="3" readonly="readonly" onkeyup="this.value=this.value.replace(/[\D]+/gi, '');" />-
<input type="text" name="serial9" id="serial9" style="width:57px;" value="<?php print_r($_POST["serial9"])?>" size="7" maxlength="7" onkeyup="this.value=this.value.replace(/[\D]+/gi, '');" />

<input type="submit" name="submit" value="Search" style="background-color:#bbddbd;font-family:arial;color:#222222;"/>

<h5><span style="font-size:12pt;color:#507630;">Please enter the Certified Emission Reduction Serial Number*</span></h5>
</br>
</center>
</form>
<?php
if (isset($_POST['submit'])) {

//--------------------------------------------------------------------Global Variables--------------------------------------------------------------------
$n = ltrim(trim($_POST["serial9"]),"0");
if($n=="")
{$n="0000";}
$srStart = ltrim(trim($_POST["serial4"]),"0");
$srEnd = ltrim(trim($_POST["serial5"]),"0");
$countryCodeI= trim($_POST["serial1"]);
//print_r($srStart." ".$srEnd );
//create a new cURL resource

$URL = "http://cdm.unfccc.int/Projects/storeSearchParameters?searchmode=advanced&titleFT=&scopeoperation=or&scale=all&methoperation=or&hostcountry=-+no+selection+-&Status=all&fromdate=&todate=&fromreduction=&toreduction=&Ref=".$n."&sort=RegistrationDate&SubmittingDOE=0&button=Search";


//print_r($n);
$ch = curl_init();
// set URL and other appropriate options
curl_setopt($ch, CURLOPT_URL,$URL );
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HEADER, 1);
$res1 = curl_exec($ch);
if(curl_errno($ch)){ 
	header("Location: /CarbonCredits_maint.php");
	//Make sure that code below does not get executed when we redirect.
	exit;
}
curl_close($ch);
preg_match('/^Set-Cookie: (.*?);/m',$res1 , $m);

$ZopeId = $m[1];
//print_r($ZopeId);

$ch = curl_init();
// set URL and other appropriate options
curl_setopt($ch, CURLOPT_URL, "http://cdm.unfccc.int/Projects/projsearch.html");
curl_setopt($ch, CURLOPT_COOKIE , $ZopeId);
curl_setopt($ch, CURLOPT_HEADER, 1);
curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
$medPage = curl_exec($ch);
if(curl_errno($ch)){ 
	header("Location: /CarbonCredits_maint.php");
	exit;
}
curl_close($ch);
//print_r($medPage);
if(preg_match('/\<a href="http:\/\/cdm\.unfccc\.int\/Projects(.*?)\>/m', $medPage, $m1))
{
//<a href="http://cdm.unfccc.int/Projects/DB/DNV-CUK1127672024.44/view">
//print_r($m1[0]);
$start=strpos($m1[0],'"')+1;
$len=strpos($m1[0],'"',10)-$start;
//print_r($start." ".$len);
$creditURL=str_replace(" ","%20",substr($m1[0],$start,$len));

//print_r($creditURL);
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, $creditURL);
curl_setopt($ch, CURLOPT_COOKIE , $ZopeId);
curl_setopt($ch, CURLOPT_HEADER, 1);
curl_setopt ($ch, CURLOPT_RETURNTRANSFER, 1);
$finalPage = curl_exec($ch);
if(curl_errno($ch)){ 
	header("Location: /CarbonCredits_maint.php");
	exit;
}
curl_close($ch);
//print_r($finalPage);

//check serial here
//get the bunch of monitoring reports-------------------------------------------
$start=strpos($finalPage,'<b>Monitoring report');
$len=strpos($finalPage,'</table>',$start)-$start;
$pName1=substr($finalPage,$start,$len);
$monDate="";
$startpoint = 0;
$countryCode = "";
$count=0;
$arrBlkStart = array();
$arrBlkEnd = array();
$arrMonDate = array();

while(1)
{
	$start=strpos($pName1,'Monitoring report',$startpoint);
	if($start==false||$start==0){
		break;
	}
	$len=strpos($pName1,'Monitoring report',$start+1)-$start;
//	get single monitoring report-------------------------------------------
	$pName2=substr($pName1,$start,$len);
//	counter; sort of-------------------------------------------------------
	$temp = $start+$len-1;
//	print_r($pName2." (");
//	get start end serial blocks--------------------------------------------
	$start=strpos($pName2,'Block start:');
	if($start==false||$start==0){
		$startpoint = $temp;
//		print_r("I am here");
		continue;
	}
	$len=strpos($pName2,'Block end',$start)-$start;
	$start1 = substr($pName2,$start,$len);
//-------------------------------Country Code Check--------------------------------
	if ($countryCode == "") {
		$start=strpos($start1,':')+1;
		$len=strpos($start1,'-',$start)-$start;
		$countryCode = substr($start1,$start,$len);
//		print_r($countryCode);
		if(strtoupper(trim($countryCode)) != strtoupper($countryCodeI)) {
			$monDate="2";
			break;
		}
	}

//-----------------------------Collect blocks and check------------------------

	$start=strpos($start1,'-')+1;
	$start=strpos($start1,'-',$start)+1;
	$len=strpos($start1,'-',$start)-$start;
	$startBlock = substr($start1,$start,$len);	
	
//	print_r($start." ".$len." startBlock:".$startBlock);

	$start=strpos($pName1,'Block end',$startpoint);
	$len=strpos($pName1,'<br>',$start)-$start;
	$end1 = substr($pName1,$start,$len);

	$start=strpos($end1 ,'-')+1;
	$start=strpos($end1 ,'-',$start)+1;
	$len=strpos($end1 ,'-',$start)-$start;
	$endBlock= substr($end1 ,$start,$len);

//	print_r($start." ".$len." endBlock:".$endBlock);

	$start=strpos($pName1,'<a ',$startpoint);
	$len = strpos($pName1,'</a>',$start+1)-$start;
	//$len=25;
	$monReport = substr($pName1,$start,$len);
//	print_r($monReport);
	if($endBlock>=$srEnd && $startBlock<=$srStart){
		$monDate = $monReport;
		break;
	}
//-----------------------------Collect blocks and check----------------------------
	$finalStart=-1;
	$finalEnd=9999999999999;
	$arrSBMatch=array();
	$arrEBMatch=array();
	$sbMatchCount=0;
	$ebMatchCount=0;

//------------------Search for multiple contiguous blocks------------------
	for ($i = 0; $i < $count; $i++) {
		if($arrBlkStart[$i]==($endBlock+1))
		{
			$arrSBMatch[$sbMatchCount]=$i;
			$sbMatchCount++;
		}
		if($arrBlkEnd[$i]==($startBlock-1))
		{
			$arrEBMatch[$ebMatchCount]=$i;
			$ebMatchCount++;
		}
	//	print_r($i);		
	}
//	print_r($ebMatchCount." ".$sbMatchCount);

//-----------------Add to block start/end array and Check------------------

//-----------------Add single report - Already checked--------------------			
	$arrBlkStart[$count] = $startBlock;
	$arrBlkEnd[$count] = $endBlock;
	$arrMonDate[$count] = $monReport;
	$count++;
//----------------Add to block start/end array and Check------------------

//----------------Add end block matches and check--------------------

	for ($i = 0; $i < $ebMatchCount; $i++) {
		//print_r(" eb ");
		$arrBlkEnd[$count] = $endBlock;
		$arrBlkStart[$count] = $arrBlkStart[$arrEBMatch[$i]];
		$arrMonDate[$count] = $arrMonDate[$arrEBMatch[$i]]."<br />".$monReport;
		if(($arrBlkEnd[$count]>=$srEnd && $arrBlkStart[$count]<=$srStart) && ($arrBlkEnd[$count]-$arrBlkStart[$count] < $finalEnd-$finalStart))
		{
			$monDate = $arrMonDate[$count];
			$finalEnd = $arrBlkEnd[$count] ;
			$finalStart = $arrBlkStart[$count];
		}
		$count++;
	}
//-----------------Add end block matches and check--------------------

//----------------Add start block matches and check-------------------
	for ($i = 0; $i < $sbMatchCount; $i++) {
		//print_r(" sb ");
		$arrBlkEnd[$count] = $arrBlkEnd[$arrSBMatch[$i]];
		$arrBlkStart[$count] = $startBlock;
		$arrMonDate[$count] = $arrMonDate[$arrSBMatch[$i]]."<br />".$monReport;
		if(($arrBlkEnd[$count]>=$srEnd && $arrBlkStart[$count]<=$srStart) && ($arrBlkEnd[$count]-$arrBlkStart[$count] < $finalEnd-$finalStart))
		{
			$monDate = $arrMonDate[$count];
			$finalEnd = $arrBlkEnd[$count] ;
			$finalStart = $arrBlkStart[$count];
		}
		$count++;
	}
//---------------Add start block matches and check-----------------

//---------------Add joined start/End block matches and check-----------------
	for ($i = 0; $i < $sbMatchCount; $i++) {
		for ($j = 0; $j < $ebMatchCount; $j++) {
			//print_r("  both  ");
			$arrBlkEnd[$count] = $arrBlkEnd[$arrSBMatch[$i]];
			$arrBlkStart[$count] = $arrBlkStart[$arrEBMatch[$j]];
			$arrMonDate[$count] = $arrMonDate[$arrEBMatch[$j]]."<br />".$arrMonDate[$arrSBMatch[$i]]."<br />".$monReport;
			if(($arrBlkEnd[$count]>=$srEnd && $arrBlkStart[$count]<=$srStart) && ($arrBlkEnd[$count]-$arrBlkStart[$count] < $finalEnd-$finalStart))
			{
				$monDate = $arrMonDate[$count];
				$finalEnd = $arrBlkEnd[$count] ;
				$finalStart = $arrBlkStart[$count];
			}
			$count++;
		}
	}
//----------------Add joined start/End block matches and check-----------------

//-------------------Add to block start/end array and Check---------------------

//---------------------Search for multiple contiguous blocks-----------------------
	if($monDate!="")
		break;

//	print_r("end");
	$startpoint = $temp;

//	for ($i = 0; $i < $count; $i++) {
//		print_r("</a>".$count." ".$arrBlkStart[$i]." : ".$arrBlkEnd[$i]." : ".$arrMonDate[$i]."\n");
//		}
}

if ($monDate=="")
{
?>
<br /><div align="center" style="font-size:12pt;color:#ff8888;">Input serial number range not found in project.</div>
<?php
} elseif ($monDate=="2")
{
?>
<br /><div align="center" style="font-size:12pt;color:#ff8888;">The Country Code for this project is <?php print_r($countryCode); ?>.</div>
<?php
} else {
?>
<style type="text/css">
table.Results {
	border-width: 2px;
	border-spacing: 2px;
	border-style: none;
	border-color: white;
	border-collapse: separate;
	background-color:#ffffff;
}
.ResultsH{
	border-width: 1px;
	padding: 2px;
	border-style: hidden;
	border-color: white;
	background-color:#6f993e;
	-moz-border-radius: 12px 12px 12px 12px;
	color:#f0f0f0;
}
.ResultsD {
	border-width: 1px;
	padding: 2px;
	border-style: hidden;
	border-color: white;
	background-color:#f2f2f2;
	-moz-border-radius: 12px 12px 12px 12px;
	color:#0f0f0f
}
</style>

<table class="Results" width = "90%" align="center">
<tr class="ResultsH">
	<td>Name of the Project</td>
	<td>CDM Reference Number</td>
	<td>Name of Project participant from Non Annex I Country</td>
	<td>Name of Project participant from Annex I Country</td>
	<td>Serial Number is in the monitoring Report Dated</td>
	<td>Does the Project appear in the ineligible CER Projects for delivery of the ICE ECX CER Futures Contract?</td>
	<td>Does the serial Number appear in the list of EUETS surrendered CERs?</td>
</tr>
<tr class="ResultsD">
	<td>
		<?php $start=strpos($finalPage,'Project title');
		$len=strpos($finalPage,'Host Parties')-$start;
		$pName1=substr($finalPage,$start,$len);
		$start=strpos($pName1,'<span>');
		$len=strpos($pName1,'</span>')-$start;
		$pName2=substr($pName1,$start,$len);
		// Project Name
		print_r($pName2);?>
	</td>
	<td align = "center">
		<?php $start=strpos($finalPage,'mH header');
		$len=strpos($finalPage,'formTable')-$start;
		$pName1=substr($finalPage,$start,$len);
		$start=strpos($pName1,'Project')+8;
		$len=strpos($pName1,':')-$start;
		$pName2=substr($pName1,$start,$len);
		//Reference Number
		print_r($pName2);?>
	</td>
	<td>
		<?php $start=strpos($finalPage,'Host Parties');
		$len=strpos($finalPage,'Other Parties Involved')-$start;
		$pName1=substr($finalPage,$start,$len);
		$flag=1;
		$startpoint=0;
		while(1){
			$start=strpos($pName1,'Participants:',$startpoint);
			if($start==false||$start==0){
				break;
			} else { 
				$start=$start+14;
			}
			if($flag==1){
				$flag=0;
			} else {
				print_r("<hr />");
			}
			$len=strpos($pName1,'</span>',$start)-$start;
			$pName2=substr($pName1,$start,$len);
			$pName2 = str_replace("<hr>", "", $pName2);
			$temp = $start+$len+1;
		//Non Annex I Participant
			print_r($pName2." (");
		
			$start=strpos($pName1,'<strong>',$startpoint);
			$len=strpos($pName1,'</strong>',$start)-$start+9;
			$pName2=substr($pName1,$start,$len);
		//Non Annex I Country
			print_r($pName2.")");
			$startpoint = $temp;
		}
		?>
	</td>
	<td>
		<?php $start=strpos($finalPage,'Other Parties Involved');
		$len=strpos($finalPage,'Activity Scale')-$start;
		$pName1=substr($finalPage,$start,$len);
		$flag=1;
		$startpoint=0;
		while(1){
			$start=strpos($pName1,'Participants:',$startpoint);
			if($start==false||$start==0){
				break;
			} else { 
				$start=$start+14;
			}
			if($flag==1){
				$flag=0;
			} else {
				print_r("<hr />");
			}
			$len=strpos($pName1,'</span>',$start)-$start;
		//print_r($start." ".$len." ".$startpoint);
			$pName2=substr($pName1,$start,$len);
			$pName2 = str_replace("<hr>", "", $pName2);
			$temp = $start+$len+1;
		//Non Annex I Participant
			print_r($pName2." (");
			$start=strpos($pName1,'<strong>',$startpoint);
			$len=strpos($pName1,'</strong>',$start)-$start+9;
		//print_r($start." ".$len);
			$pName2=substr($pName1,$start,$len);
		//Non Annex I Country
			print_r($pName2.")");
			$startpoint = $temp;
		}
		?>
	</td>
	<td>
		<?php print_r($monDate);?>
	</td>
	<td align="center"><?php
function xml2assoc($xml, $n)
{ 
    while($xml->read()) 
    {
        if($xml->nodeType == XMLReader::END_ELEMENT)
        {
            return 0;
        }
        
        else if($xml->nodeType == XMLReader::ELEMENT)
        {
            //print_r($xml->name);           
            if(!$xml->isEmptyElement)
            {
                if(xml2assoc($xml, $n)==1)
			return 1;
            }
        }
        
        else if($xml->nodeType == XMLReader::TEXT)
        {
//		print_r("<br />".ltrim(trim($xml->value),"0")." ".$n);
		if(ltrim(trim($xml->value),"0") == $n)
			return 1;
        }
    }
    return 0;
}

$xml = new XMLReader(); 
$xml->open('Ineligible CERS.xml'); 
if(xml2assoc($xml, $n)==1){
?>
<span style="font-size:12pt;color:red;font-weight:bold;">Yes</span>
<?php
} else {
?>
<span style="font-size:12pt;color:green;font-weight:bold;">No</span>
<?php 
}
$xml->close();
	?></td align="center">
	<td>--We are working on it--
	</td>
	
</tr>
</table>
<?php
}
} else {
?>
<br /><div align="center" style="font-size:12pt;color:#ff8888;">No Matching Project Found</div>
<?php
}
//} else {
?>
<!--<br /><div align="center" style="font-size:12pt;color:#ff8888;">Unit type is not CER</div>-->
<?php
//}
//onkeypress="if(event.keyCode/*!this.value.match(/^[A-Za-z]*$/)*/){return false;}"
}
?>

<center>
<br />
<h1> </h1>
<br />
<h1> </h1>
<br />
<span style="font-family:Calibri,Verdana;font-size:10pt;color:#507630;">
*You will see the serial number of CER in you registry holding account.
<br />
&#149;<a id="disclaimer" href="#disclaimer" onclick="var stl=document.getElementById('disc1').style.display; if(stl=='none') document.getElementById('disc1').style.display='block'; else document.getElementById('disc1').style.display='none';"> Disclaimer </a>&#149;
<div id="disc1" style="font-size:12pt;color:#888888;display:none;width=60%;"><p width="60%">Information contained on the web site is made available for general information only. Climate Connect shall not be liable to you or any other person for any losses liabilities or damages (whether direct indirect consequential or otherwise) whatsoever arising directly or indirectly from or in connection with (i) any use by you of the web site or information contained in the web site from time to time or (ii) any interruption or delay in access to the web site (iii) any error, omission or inaccuracy in or incompleteness of any information displayed in the web site or (iv) any defect, delay or interruption in any communication or data processing equipment software services or media, and however caused or arising whether due in whole or in part to Climate Connect 's negligence or otherwise, and whether or not Climate Connect is advised of the possibility of damages.
</p></div> 
<br />
For any queries please email  
</span>

<A HREF="mailto:serialnumber@climate-connect.co.uk">
<span style="font-family:Calibri,Verdana;font-size:10pt;">info@climate-connect.co.uk</span>
</A>
</center>
</body>
</html>
