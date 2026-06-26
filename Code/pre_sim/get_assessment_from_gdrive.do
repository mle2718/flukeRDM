* stock assessment numbers-at-age data
	* Min-Yang processes the historical numbers-at-age data and makes projections, and stores his output in Google Drive
	* Here I pull that data from Google Drive (using the Desktop app file path) and save it with a generic name in a local folder 



local google_folder "D:/Shared drives/NMFS NEC READ SSB/socialsci/RecreationalDST/2028_management_cycle_data/flukeRDM/input_data"

/* will need to adjust this to match actual file names*/
local filestubs  "fit_NAA_NORTH fit_NAA_SOUTH fit_proj_NAA_NORTH fit_proj_NAA_SOUTH J1_2024Scup J1_2026Scup J1_2024Summer_Flounder J1_2026Summer_Flounder"

foreach s of local filestubs {
    clear
    local files : dir "`google_folder'" files "`s'_*.dta" // find matching file
    local last: word count `files'
	local myfile : word `last' of `files' // grab last match
    di "`myfile'"
	local myfile : subinstr local myfile `"""' "", all // remove embedded quotes
    local fullpath `"`google_folder'/`myfile'"' // build full path
    di as text "Loading: `fullpath'" 
	copy "`fullpath'" `"$misc_data_cd/`s'.dta"' , replace //copy files from google drive to misc_data_cd
}

