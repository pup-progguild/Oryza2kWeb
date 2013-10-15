<?php
 /**
 * Super Class
 *
 * @package	    Errors
* @subpackage	Subpackage
* @category	Category
* @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 2:14 AM
*/

class Errors extends CI_Controller {
    public function page_missing() {
        $this->load->library('user_agent');
        $this->load->helper('url');

        $this->load->view('utilities/page_pissing');
    }
}