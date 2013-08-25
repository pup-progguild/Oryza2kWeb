<?php
 /**
 * About
 *
 * @package	    About
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/24/13 | 11:27 PM
 */

class About extends CI_Controller {
    public function index() {
        $data['title'] = 'About';

        $this->load->view('templates/header');
        $this->load->view('templates/content-start');
        $this->load->view('pages/about');
        $this->load->view('templates/content-end');
        $this->load->view('templates/footer');
    }
}