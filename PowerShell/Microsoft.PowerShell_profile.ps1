# Produce UTF-8 by default
$PSDefaultParameterValues["Out-File:Encoding"] = "utf8"

# https://technet.microsoft.com/en-us/magazine/hh241048.aspx
$MaximumHistoryCount = 10000;

function source {
	if (Test-Path $PROFILE) {
		Write-Verbose "Sourcing $PROFILE"
		. $PROFILE
	}
}

function explorer {
	explorer.exe .
}

function limit-HomeDirectory($Path) {
	$Path.Replace("$home", "~")
}

function prompt {
	$prompt = ""

	$prompt += $(limit-HomeDirectory("$pwd")) + "`n"
	$prompt += "$ "

	$prompt
}