function myip -d "Provides my current IP."
	command dig +short myip.opendns.com @resolver1.opendns.com $argv
end
