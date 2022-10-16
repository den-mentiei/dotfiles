function grt -d "Changes to the current repository root."
	cd (git rev-parse --show-toplevel; or echo ".")
end
