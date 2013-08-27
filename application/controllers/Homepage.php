<?php
/**
 * Created by IntelliJ IDEA.
 * User: hoshi~
 * Date: 8/24/13
 * Time: 5:29 PM
 * To change this template use File | Settings | File Templates.
 */
class Homepage extends CI_Controller {
    public function index() {
        $data['title'] = 'Home';

        $this->load->view('templates/header', $data);
        $this->load->view('templates/content-start', $data);
        $this->load->view('pages/home', $data);
        $this->load->view('templates/content-end', $data);
        $this->load->view('templates/footer', $data);
    }
}