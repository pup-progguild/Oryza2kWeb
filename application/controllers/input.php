<?php
 /**
 * Input
 *
 * @package	    Input
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/24/13 | 11:25 PM
 */

class Input extends CI_Controller {
    public function index() {
        $data['title'] = 'Input';

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/input', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }
}