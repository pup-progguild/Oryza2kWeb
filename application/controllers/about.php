<?php
 /**
 * Super Class
 *
 * @package	    
 * @subpackage	Subpackage
 * @category	Category
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 3:21 AM
 */

class About extends CI_Controller {
    public function __construct() {
        $this->load->helper('url');
    }

    public function index() {
        $data['title'] = 'About';

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/about', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }
}